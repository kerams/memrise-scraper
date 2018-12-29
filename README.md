## memrise-scraper
Dumps data from all of the columns (i.e. alts, audio links, custom columns, ...) in the first database into a CSV file, without the need to log in.

### Usage
- Download `MemriseScraper.zip` from [releases](http://github.com/kerams/memrise-scraper/releases/latest) and extract it somewhere.
- Get the ID of the course to scrape ![](https://i.imgur.com/wLRjrwc.png)
- Using the command line, pass the course ID to `MemriseScraper.exe` ![](https://i.imgur.com/V487451.png)
- Dots will be printed on the screen for every level processed. The entire process may take a couple of minutes for large courses because Memrise's API is very slow.
- Once the process is done, the CSV is located in the same folder as the exe.

### Current limitations
- Does not work with courses that have multimedia levels
- Windows only
