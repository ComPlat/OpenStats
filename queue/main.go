package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"net"
	"os"
	"runtime"
	"strconv"
	"strings"
	"sync"
)

// -------------------- Helpers --------------------

func processString(s string) string {
	return strings.TrimSpace(strings.ReplaceAll(s, "\n", ""))
}

// -------------------- Types --------------------

type Session struct {
	id           string
	cores        int
	priority     int
	desiredCores int
}

// -------------------- Status --------------------

const (
	OK              = "OK"
	TOOMANYCORES    = "Exceeded core limit"
	CORESALLOCATED  = "Cores allocated"
	NOTENOUGHCORES  = "Not enough cores"
	SESSIONNOTFOUND = "Session not found"
)

// -------------------- Globals --------------------

var totalCores = max(1, runtime.NumCPU()-1)
var usedCores = 0

var shinySessions = make(map[string]*Session)
var mutex = &sync.Mutex{}

// -------------------- Core Logic --------------------

func requestCores(session *Session) string {
	if session.desiredCores > totalCores {
		return TOOMANYCORES
	}

	if session.desiredCores <= (totalCores - usedCores) {
		session.cores = session.desiredCores
		usedCores += session.cores
		return CORESALLOCATED
	}

	return NOTENOUGHCORES
}

func releaseCores(session *Session) {
	usedCores -= session.cores
	if usedCores < 0 {
		usedCores = 0
	}
	session.cores = 0
}

// -------------------- Scheduler --------------------

func schedule() {
	for {
		id := getHighestPriority()
		if id == "" {
			return
		}

		session := shinySessions[id]
		status := requestCores(session)
		if status != CORESALLOCATED {
			return
		}

		// reset priority after scheduling
		session.priority = -1
	}
}

func getHighestPriority() string {
	maxPrio := -1
	var bestID string

	for id, s := range shinySessions {
		if s.priority > maxPrio &&
			s.desiredCores <= (totalCores-usedCores) {
			maxPrio = s.priority
			bestID = id
		}
	}

	return bestID
}

// -------------------- Command Handling --------------------

func handleInfo(info string) (string, error) {
	mutex.Lock()
	defer mutex.Unlock()

	split := strings.Split(info, ":")
	if len(split) < 2 || len(split) > 3 {
		return OK, errors.New("invalid input")
	}

	for i := range split {
		split[i] = processString(split[i])
	}

	cmd := split[0]
	id := split[1]

	switch cmd {

	case "add":
		shinySessions[id] = &Session{
			id:       id,
			priority: -1,
		}
		return OK, nil

	case "remove":
		session, ok := shinySessions[id]
		if !ok {
			return SESSIONNOTFOUND, nil
		}
		releaseCores(session)
		delete(shinySessions, id)
		schedule()
		return OK, nil

	case "request":
		if len(split) != 3 {
			return OK, errors.New("missing core request")
		}

		session, ok := shinySessions[id]
		if !ok {
			return SESSIONNOTFOUND, nil
		}

		cores, err := strconv.Atoi(split[2])
		if err != nil {
			return OK, err
		}

		session.desiredCores = cores

		status := requestCores(session)
		if status == NOTENOUGHCORES {
			session.priority++
		}

		return status, nil

	case "release":
		session, ok := shinySessions[id]
		if !ok {
			return SESSIONNOTFOUND, nil
		}

		releaseCores(session)
		schedule()
		return OK, nil
	}

	return OK, nil
}

// -------------------- TCP --------------------

func readRInfo(conn net.Conn, errCh chan<- error) string {
	reader := bufio.NewReader(conn)
	message, err := reader.ReadString('\n')
	if err != nil {
		errCh <- err
		return OK
	}

	status, err := handleInfo(message)
	if err != nil {
		errCh <- err
	}
	return status
}

func sendRInfo(conn net.Conn, info string) error {
	_, err := fmt.Fprintln(conn, info)
	return err
}

func runTCP(errCh chan<- error) {
	ln, err := net.Listen("tcp", ":9091")
	if err != nil {
		fmt.Println("FATAL: cannot listen:", err)
		errCh <- err
		return
	}
	fmt.Println("Listening on :9091")
	defer ln.Close()

	for {
		conn, err := ln.Accept()
		if err != nil {
			errCh <- err
			continue
		}

		go func(c net.Conn) {
			defer c.Close()
			status := readRInfo(c, errCh)
			if err := sendRInfo(c, status); err != nil {
				errCh <- err
			}
		}(conn)
	}
}

// -------------------- Main --------------------

func main() {
	errCh := make(chan error)

	logFile, err := os.OpenFile("log.txt", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatal("Failed to open log file:", err)
	}
	log.SetOutput(logFile)

	go func() {
		for err := range errCh {
			if err != nil {
				log.Println("Error:", err)
			}
		}
	}()

	runTCP(errCh)
}
