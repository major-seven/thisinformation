package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"strings"
	"github.com/mmcdole/gofeed"
)

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

	// fetch news titles
	urls := make([]string, 0)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		urls = append(urls, scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		panic(err)
    }
	titles, err := fetch_titles_from_rss_urls(urls)
	if err != nil {
		panic(err)
	}
	titles_filtered := filter_titles(titles)
	if false {
		fmt.Println(titles_filtered)
	}
	
	prompt := ArticlePrompt{
		Model: "reporter",
		Prompt: "write an article for a newspaper",
		Stream: false,
	}

	content, err := send_article_prompt(prompt)
	if err != nil {
		panic(err)
	}
	fmt.Println(content)

}

func fetch_titles_from_rss_urls(urls []string) ([]string, error) {
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

func filter_titles(all []string) []string {
	result := make([]string, 0)
	for i := 0; i < len(all); i++ {
		if !strings.Contains(all[i], "NOVA") {
			result = append(result, all[i])
		}
	}
	return result
}

func create_article_from_original_title(title *string) string {
	return ""
}

func send_article_prompt(prompt ArticlePrompt) (string, error) {
	json_data, err := json.Marshal(prompt)
	req, err := http.NewRequest(
		"POST",
		"http://localhost:11434/api/generate",
		bytes.NewBuffer(json_data),
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
	return "", nil
}
