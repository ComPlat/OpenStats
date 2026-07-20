const { chromium } = require('playwright');
const fs = require('fs');

async function setSelectize(page, inputId, value) {
  await page.click(`#${inputId}-selectized`);
  await page.fill(`#${inputId}-selectized`, value);
  await page.waitForTimeout(250);
  await page.keyboard.press('Enter');
  await page.waitForTimeout(250);
}

(async () => {
  const N = parseInt(process.argv[2] || '20', 10);
  const PORT = process.env.LOADTEST_PORT || '3839';
  const CSV = process.env.LOADTEST_CSV || '../test_data/DoseResponseData.csv';
  const log = fs.createWriteStream('repeated_log.csv');
  log.write('iter,elapsed_s,status_final\n');

  const browser = await chromium.launch();
  const page = await browser.newPage();
  await page.goto(`http://127.0.0.1:${PORT}/`, { waitUntil: 'load', timeout: 30000 });
  await page.waitForTimeout(1500);
  await page.setInputFiles('#DOWNLOAD-file', CSV);
  await page.waitForTimeout(2000);

  await page.click('button:has-text("Open formula editor")');
  await page.waitForTimeout(700);
  await setSelectize(page, 'FO-colnames-dropdown_', 'abs');
  await page.click('#FO-conc');
  await page.waitForTimeout(300);
  await page.click('#FO-create_formula');
  await page.waitForTimeout(1500);
  await page.click('button:has-text("Close")');
  await page.waitForTimeout(300);

  await page.click('a:has-text("Dose Response analysis")');
  await page.waitForTimeout(500);
  await setSelectize(page, 'DOSERESPONSE-unitNames', 'units');
  await setSelectize(page, 'DOSERESPONSE-substanceNames', 'substance');
  await setSelectize(page, 'DOSERESPONSE-type', 'continuous');

  console.log(`=== running ${N} sequential analyses in one session ===`);
  for (let i = 1; i <= N; i++) {
    const t0 = Date.now();
    await page.click('#DOSERESPONSE-ic50');
    let status = '';
    for (let s = 0; s < 60; s++) {
      await page.waitForTimeout(500);
      status = await page.$eval('#running_status', el => el.textContent.trim()).catch(() => '');
      if (status === '' || /idle|done|complete/i.test(status)) break;
    }
    const elapsed = (Date.now() - t0) / 1000;
    console.log(`iter ${i}: elapsed=${elapsed.toFixed(2)}s status="${status}"`);
    log.write(`${i},${elapsed.toFixed(2)},${status}\n`);
  }
  log.end();
  await browser.close();
})();
