# thisinformation

![image](screenshot.png)

> [!CAUTION]
> This project is not meant to be used as a news source

Create reporter ollama model:
`$ ollama create reporter -f ollama/ReporterModelfile`

- The news producer runs once a day
- It generates all articles using a local LLM and uses the Open AI API to generate images
- After it created the new edition, it is uploaded to the server's DB
- The web-server shows today's edition as the landing page
- There also is an archive with all old editions
- Daily visitors are counted by the server on page load
