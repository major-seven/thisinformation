package rss

import (
	"bufio"
	"math/rand"
	"os"
	"strings"

	"github.com/mmcdole/gofeed"
)

func GetSourcesFromFile(fileName string) ([]string, error) {
	file, err := os.Open(fileName)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	urls := make([]string, 0)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		urls = append(urls, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return urls, nil
}

func FetchTitlesFromRssUrls(urls []string) ([]string, error) {
	titles := make([]string, 0)

	for _, url := range urls {
		fp := gofeed.NewParser()
		feed, err := fp.ParseURL(url)
		if err != nil {
			return nil, err
		}

		for _, item := range feed.Items {
			titles = append(titles, item.Title)
		}
	}

	return filterTitles(titles), nil
}

func filterTitles(titles []string) []string {
	result := make([]string, 0)

	for _, title := range titles {
		lowerTitle := strings.ToLower(title)

		if strings.Contains(title, "NOVA") {
			continue
		}
		if strings.Contains(lowerTitle, "watch the program") {
			continue
		}
		if strings.Contains(lowerTitle, "guardian") {
			continue
		}
		if strings.Contains(lowerTitle, "the guardian") {
			continue
		}
		if strings.Contains(lowerTitle, "cnn") {
			continue
		}
		if strings.Contains(lowerTitle, "nytimes") {
			continue
		}
		if strings.Contains(lowerTitle, "new york times") {
			continue
		}
		if strings.Contains(lowerTitle, "ny times") {
			continue
		}
		if strings.Contains(lowerTitle, "pbs") {
			continue
		}
		result = append(result, title)
	}

	return result
}

func SelectNRandom(all []string, n int) []string {
	rand.Shuffle(len(all), func(i, j int) { all[i], all[j] = all[j], all[i] })
	numberOfArticles := min(n, len(all))
	return all[:numberOfArticles]
}
