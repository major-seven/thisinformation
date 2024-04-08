package main

import (
	"flag"
	"fmt"
	"os"
	"strings"

	"github.com/major-seven/thisinformation/news-produce/article"
	"github.com/major-seven/thisinformation/news-produce/ollama"
	"github.com/major-seven/thisinformation/news-produce/rss"
)

type Config struct {
	NumberOfArticlesTarget int
	SubselectionSize       int
	ModelName              string
	SourcesFile            string
	ServerKey              string
}

func main() {
	var (
		NumberOfArticlesTarget int
		SubselectionSize       int
		ModelName              string
		SourcesFile            string
		Verbose                bool
		ServerKey              string
	)

	ServerKey = os.Getenv("NEWS_KEY")

	flag.IntVar(&NumberOfArticlesTarget, "n", 10, "Number of articles to generate")
	flag.IntVar(&SubselectionSize, "sub", 50, "Number of random titles model can select from")
	flag.StringVar(&ModelName, "model", "news", "Model to use for generation")
	flag.StringVar(&SourcesFile, "src", "sources.txt", "File containing list of RSS sources")
	flag.BoolVar(&Verbose, "v", false, "Verbose output")
	flag.StringVar(&ServerKey, "key", "", "Server key for API, if not set will use NEWS_KEY env var")
	flag.Parse()

	config := Config{
		NumberOfArticlesTarget: NumberOfArticlesTarget,
		SubselectionSize:       SubselectionSize,
		ModelName:              ModelName,
		SourcesFile:            SourcesFile,
		ServerKey:              ServerKey,
	}

	fmt.Printf("Generating %d articles using model %s from sources in %s\n", config.NumberOfArticlesTarget, config.ModelName, config.SourcesFile)

	urls, err := rss.GetSourcesFromFile(config.SourcesFile)
	if err != nil {
		panic(err)
	}

	fmt.Printf("\nFound %d sources:\n", len(urls))
	for i, url := range urls {
		fmt.Printf("%d: %s\n", i+1, url)
	}

	titles, err := rss.FetchTitlesFromRssUrls(urls)
	if err != nil {
		panic(err)
	}

	fmt.Printf("\nFound %d titles\n", len(titles))
	if Verbose {
		for i, title := range titles {
			fmt.Printf("%d: %s\n", i+1, title)
		}
	}

	fmt.Printf("\nSelecting %d random titles for model to consider...\n", config.SubselectionSize)
	titles = rss.SelectNRandom(titles, config.SubselectionSize)
	if Verbose {
		for i, title := range titles {
			fmt.Printf("%d: %s\n", i+1, title)
		}
	}

	fmt.Printf("\nLetting model select %d titles...\n", config.NumberOfArticlesTarget)
	selectedTitles, err := ollama.SelectNArticlesFromTitles(config.NumberOfArticlesTarget, titles)
	if err != nil {
		panic(err)
	}

	fmt.Printf("\nModel selected %d titles\n", len(selectedTitles))
	if Verbose {
		for i, title := range selectedTitles {
			fmt.Printf("%d: %s\n", i+1, title)
		}
	}

	fmt.Printf("\nGenerating articles...\n")
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
		err = article.AddToServer(config.ServerKey)
		if err != nil {
			fmt.Printf("Error adding article to server: %s\n", err)
			continue
		}

		numArticles += 1
	}

	fmt.Printf("\nGenerated %d articles\n", numArticles)
}
