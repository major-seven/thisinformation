package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"math/rand"
	"strings"
	"time"
	"github.com/mmcdole/gofeed"
)

type Config struct {
	NumberOfArticlesTarget int
}

type ArticlePrompt struct {
	Model string	`json:"model"`
	Prompt string	`json:"prompt"`
	Stream bool		`json:"stream"`
}

type ArticlePromptResponse struct {
	Model string		`json:"model"`
	CreatedAt string	`json:"created_at"`
	Response string		`json:"response"`
	Done bool			`json:"done"`
}

func main() {
	// set config
	config := Config{ 3 }

	// read in sources
	args := os.Args[1:]
	if len(args) != 1 {
		log.Println("usage: './news_produce <source-file.txt>'")
		return
	}
	file, err := os.Open(args[0])
	if err != nil {
		panic(err)
	}
	defer file.Close()

	urls := make([]string, 0)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		urls = append(urls, scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		panic(err)
    }

	// fetch news titles
	titles, err := fetchTitlesFromRssUrls(urls)
	if err != nil {
		panic(err)
	}
	titles = filterTitles(titles)
	titles = selectNTitles(titles, config.NumberOfArticlesTarget)

	for _, t := range titles {
		fmt.Println(t)
		fmt.Println(createArticleFromOriginalTitle(&t))
	}
	
}

func fetchTitlesFromRssUrls(urls []string) ([]string, error) {
	titles := make([]string, 0)
	for u := 0; u < len(urls); u += 1 {
		fp := gofeed.NewParser()
		feed, err := fp.ParseURL(urls[u])
		if err != nil {
			return nil, err
		}

		for i := 0; i < len(feed.Items); i += 1 {
			titles = append(titles, feed.Items[i].Title)
		}
	}
	return titles, nil
}

func filterTitles(all []string) []string {
	result := make([]string, 0)
	for i := 0; i < len(all); i++ {
		if !strings.Contains(all[i], "NOVA") {
			result = append(result, all[i])
		}
	}
	return result
}

func selectNTitles(all []string, n int) []string {
	rand.Seed(time.Now().UnixNano()) // needed for unique seed
	rand.Shuffle(len(all), func (i, j int) { all[i], all[j] = all[j], all[i] })
	numberOfArticles := min(n, len(all))
	return all[:numberOfArticles]
}

func createArticleFromOriginalTitle(title *string) string {
	prompt := ArticlePrompt{
		Model: "news",
		Prompt: `
		write an article for a newspaper based on the following title: ` + *title,
		Stream: false,
	}

	content, err := sendArticlePrompt(prompt)
	if err != nil {
		panic(err)
	}
	return content
}

func sendArticlePrompt(prompt ArticlePrompt) (string, error) {
	jsonData, err := json.Marshal(prompt)
	req, err := http.NewRequest(
		"POST",
		"http://localhost:11434/api/generate",
		bytes.NewBuffer(jsonData),
	)
	if err != nil {
		return "", err
	}
	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	article := ""
	if resp.StatusCode == http.StatusOK {
		bodyBytes, err := io.ReadAll(resp.Body)
		if err != nil {
			log.Fatal(err)
		}
		var response ArticlePromptResponse
		if err := json.Unmarshal(bodyBytes, &response); err != nil {
			return "", err
		}
		article = response.Response
		return article, nil
	}
	return "", errors.New("Prompt to LLM failed")
}
