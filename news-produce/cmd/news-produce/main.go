package main

import (
	"flag"
	"fmt"
	"os"
	"strings"

	"github.com/major-seven/thisinformation/news-produce/internal/article"
	"github.com/major-seven/thisinformation/news-produce/internal/dalle"
	"github.com/major-seven/thisinformation/news-produce/internal/ollama"
	"github.com/major-seven/thisinformation/news-produce/internal/rss"
)

type Config struct {
	NumberOfArticlesTarget int
	SubselectionSize       int
	ModelName              string
	SourcesFile            string
	ServerKey              string
	ServerUrl              string
	CreateImage            bool
	ImagePath              string
	CreatePuzzle           bool
	PuzzlePath             string
	PostTest               bool
}

func main() {
	var (
		NumberOfArticlesTarget int
		SubselectionSize       int
		ModelName              string
		SourcesFile            string
		Verbose                bool
		ServerKey              string
		ServerUrl              string
		CreateImage            bool
		ImagePath              string
		CreatePuzzle           bool
		PuzzlePath             string
		PostTest               bool
	)

	ServerKey = os.Getenv("NEWS_KEY")

	flag.IntVar(&NumberOfArticlesTarget, "n", 10, "Number of articles to generate")
	flag.IntVar(&SubselectionSize, "sub", 50, "Number of random titles model can select from")
	flag.StringVar(&ModelName, "model", "news", "Model to use for generation")
	flag.StringVar(&SourcesFile, "src", "sources.txt", "File containing list of RSS sources")
	flag.BoolVar(&Verbose, "v", false, "Verbose output")
	flag.StringVar(&ServerKey, "key", ServerKey, "Server key for API, if not set will use NEWS_KEY env var")
	flag.BoolVar(&CreateImage, "img", false, "Create image for first title")
	flag.StringVar(&ImagePath, "imgpath", "", "Path to save image")
	flag.BoolVar(&CreatePuzzle, "puz", false, "Create daily puzzle")
	flag.StringVar(&PuzzlePath, "puzpath", "", "Path to save puzzle")
	flag.StringVar(&ServerUrl, "url", "http://localhost:3000", "Server URL")
	flag.BoolVar(&PostTest, "posttest", false, "Test posting an article to the server.")
	flag.Parse()

	config := Config{
		NumberOfArticlesTarget: NumberOfArticlesTarget,
		SubselectionSize:       SubselectionSize,
		ModelName:              ModelName,
		SourcesFile:            SourcesFile,
		ServerKey:              ServerKey,
		ServerUrl:              ServerUrl,
		CreateImage:            CreateImage,
		CreatePuzzle:           CreatePuzzle,
		PuzzlePath:             PuzzlePath,
		ImagePath:              ImagePath,
		PostTest:               PostTest,
	}

	if config.PostTest {
		fmt.Printf("Testing posting an article to the server...\n")
		article := article.Article{
			Title:   "Test Title",
			Content: "Test Content",
			Author:  "Test Author",
		}
		err := article.AddToServer(config.ServerKey, config.ServerUrl)
		if err != nil {
			panic(err)
		}
		fmt.Printf("Posted test article to server\n")
		return
	}

	fmt.Printf("Generating %d articles using model %s from sources in %s\n", config.NumberOfArticlesTarget, config.ModelName, config.SourcesFile)

	urls, err := rss.GetSourcesFromFile(config.SourcesFile)
	if err != nil {
		panic(err)
	}

	fmt.Printf("Found %d sources:\n", len(urls))
	for i, url := range urls {
		fmt.Printf("%d: %s\n", i+1, url)
	}

	titles, err := rss.FetchTitlesFromRssUrls(urls)
	if err != nil {
		panic(err)
	}

	fmt.Printf("Found %d titles\n", len(titles))
	if Verbose {
		for i, title := range titles {
			fmt.Printf("%d: %s\n", i+1, title)
		}
	}

	fmt.Printf("Selecting %d random titles for model to consider...\n", config.SubselectionSize)
	titles = rss.SelectNRandom(titles, config.SubselectionSize)
	if Verbose {
		for i, title := range titles {
			fmt.Printf("%d: %s\n", i+1, title)
		}
	}

	fmt.Printf("Letting model select %d titles...\n", config.NumberOfArticlesTarget)
	selectedTitles, err := ollama.SelectNArticlesFromTitles(config.NumberOfArticlesTarget, titles)
	if err != nil {
		panic(err)
	}

	fmt.Printf("Model selected %d titles\n", len(selectedTitles))
	if Verbose {
		for i, title := range selectedTitles {
			fmt.Printf("%d: %s\n", i+1, title)
		}
	}

	if config.CreateImage {
		fmt.Printf("Creating image for '%s'...\n", selectedTitles[0])
		prompt, err := ollama.CreateImagePromptFromTitle(selectedTitles[0])
		if err != nil {
			panic(err)
		}

		if Verbose {
			fmt.Printf("Prompt: %s\n", prompt)
		}

		url, err := dalle.CreateImageForPrompt(prompt)
		if err != nil {
			panic(err)
		}

		fmt.Printf("Saving image to %s\n", ImagePath)
		dalle.DownloadImage(url, ImagePath)
	}

	fmt.Printf("Generating articles...\n")
	numArticles := 0
	for _, title := range selectedTitles {
		article := article.Article{}

		if Verbose {
			fmt.Printf("\n%s\n", strings.Repeat("-", 30))
			fmt.Printf("Old title: %s\n", title)
		}

		content, err := ollama.CreateArticleFromTitle(&title, &config.ModelName)
		if err != nil {
			fmt.Printf("Error creating article from title: %s\n", err)
			continue
		}
		article.Content = content

		if Verbose {
			fmt.Printf("%s\n", strings.Repeat("-", 30))
			fmt.Printf("%s\n", content)
		}

		newTitle, err := ollama.CreateTitleFromArticle(&article.Content)
		if err != nil {
			fmt.Printf("Error creating title from article: %s\n", err)
			continue
		}
		article.Title = newTitle
		article.TrimTitle()

		if Verbose {
			fmt.Printf("%s\n", strings.Repeat("-", 30))
			fmt.Printf("%s\n", article.Title)
		}

		author, err := ollama.CreateAuthorFromArticle(&article.Content)
		if err != nil {
			fmt.Printf("Error creating author from article: %s\n", err)
			continue
		}
		article.Author = author
		article.TrimAuthor()

		if Verbose {
			fmt.Printf("%s\n", strings.Repeat("-", 30))
			fmt.Printf("By %s\n\n", article.Author)
			fmt.Printf("%s", strings.Repeat("#", 30))
		}

		fmt.Printf("\nGenerated article: %s\n", article.Title)

		fmt.Printf("Adding article to server...\n")
		err = article.AddToServer(config.ServerKey, config.ServerUrl)
		if err != nil {
			fmt.Printf("Error adding article to server: %s\n", err)
			continue
		}

		numArticles += 1
	}

	fmt.Printf("\nGenerated %d articles\n", numArticles)

	if config.CreatePuzzle {
		fmt.Printf("Creating daily puzzle...\n")
		url, err := dalle.GenerateDailyPuzzle()
		if err != nil {
			panic(err)
		}
		fmt.Printf("Saving daily puzzle to %s...\n", config.PuzzlePath)
		dalle.DownloadImage(url, config.PuzzlePath)

		fmt.Printf("Creating puzzle hint...\n")
		hints, err := ollama.CreateCrosswordHints()
		if err != nil {
			panic(err)
		}
		puzzleArticle := article.Article{
			Title:   "CROSSWORD",
			Content: hints,
			Author:  "CROSSWORD",
		}
		puzzleArticle.AddToServer(config.ServerKey, config.ServerUrl)
	}
}
