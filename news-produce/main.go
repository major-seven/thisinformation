package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"github.com/mmcdole/gofeed"
)

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
		fmt.Println(scanner.Text())
		urls = append(urls, scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		panic(err)
    }
	titles, err := fetch_titles_from_rss_urls(urls)
	if err != nil {
		panic(err)
	}
	fmt.Println(titles)
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
	return nil
}

func create_article_from_original_title(title *string) string {
	return ""
}
