Shiny.addCustomMessageHandler('downloadZip', function(message) {
  var FileContent = message.FileContent;
  var FileName = message.Filename;
  var ResultNames = message.ResultNames;

  if( (typeof FileContent) == "string") {
    if (FileContent.startsWith("data:image")) {
      var fileName = ResultNames + '.png';
      var zip = new JSZip();
      var imageData = atob(FileContent.split(',')[1]);
      var byteArray = new Uint8Array(imageData.length);
      for (var i = 0; i < imageData.length; i++) {
        byteArray[i] = imageData.charCodeAt(i);
      }
      zip.file(fileName, byteArray, {binary: true});
      zip.generateAsync({type: 'blob'}).then(function(content) {
        saveAs(content, FileName);
      });
    } else {
      var zipText = new JSZip();
      var fileName = ResultNames + '.txt';
      zipText.file(fileName, FileContent);
      zipText.generateAsync({type: 'blob'}).then(function(content) {
        saveAs(content, FileName);
      });
    }
  } else {
    var zip = new JSZip();
    for (var i in FileContent) {
      if (FileContent[i].startsWith("data:image")) {
        var fileName = ResultNames[i] + '.png';
        var imageData = atob(FileContent[i].split(',')[1]);
        var byteArray = new Uint8Array(imageData.length);
        for (var j = 0; j < imageData.length; j++) {
          byteArray[j] = imageData.charCodeAt(j);
        }
        zip.file(fileName, byteArray, {binary: true});
      } else {
        var fileName = ResultNames[i] + '.txt';
        zip.file(fileName, FileContent[i]); 
      }
    }
    zip.generateAsync({type: 'blob'}).then(function(content) {
      saveAs(content, FileName);
    });
  }
});

Shiny.addCustomMessageHandler("downloadExcel", function(message) {
  const byteCharacters = atob(message.fileContent);
  const byteNumbers = new Array(byteCharacters.length);
  for (let i = 0; i < byteCharacters.length; i++) {
    byteNumbers[i] = byteCharacters.charCodeAt(i);
  }
  const byteArray = new Uint8Array(byteNumbers);
  const blob = new Blob([byteArray], { type: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" });
  const link = document.createElement('a');
  link.href = window.URL.createObjectURL(blob);
  link.download = message.filename;
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
});

