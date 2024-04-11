package article

import (
	"bytes"
	"encoding/json"
	"errors"
	"net/http"
	"strings"
)

type Article struct {
	Title   string `json:"title"`
	Content string `json:"content"`
	Author  string `json:"author"`
	Date    string `json:"date"`
}

func (a *Article) TrimTitle() {
	if a.Title != "" {
		a.Title = strings.TrimSpace(a.Title)

		if strings.HasPrefix(a.Title, "\"") && strings.HasSuffix(a.Title, "\"") {
			a.Title = strings.TrimPrefix(a.Title, "\"")
			a.Title = strings.TrimSuffix(a.Title, "\"")
		}

		if strings.HasPrefix(a.Title, "'") && strings.HasSuffix(a.Title, "'") {
			a.Title = strings.TrimPrefix(a.Title, "'")
			a.Title = strings.TrimSuffix(a.Title, "'")
		}

		if strings.HasPrefix(a.Title, "Title:") {
			a.Title = strings.TrimPrefix(a.Title, "Title:")
		}

		a.Title = strings.TrimSpace(a.Title)
	}
}

func (a *Article) TrimAuthor() {
	if a.Author != "" {
		if strings.Contains(a.Author, "by") {
			a.Author = strings.Split(a.Author, "by")[1]
		}

		if strings.Contains(a.Author, "By") {
			a.Author = strings.Split(a.Author, "By")[1]
		}

		if strings.Contains(a.Author, ":") {
			a.Author = strings.Split(a.Author, ":")[1]
		}

		if strings.Contains(strings.ToLower(a.Author), "sure") {
			split := strings.Split(a.Author, " ")
			if len(split) > 2 {
				a.Author = strings.Join(split[len(split)-2:], " ")
			}
		}

		if strings.HasSuffix(a.Author, ".") {
			a.Author = strings.TrimSuffix(a.Author, ".")
		}

		a.Author = strings.TrimSpace(a.Author)
	}
}

func (a *Article) AddToServer(key, serverUrl string) error {
	jsonData, err := json.Marshal(a)
	if err != nil {
		return err
	}

	resp, err := http.Post(
    serverUrl+"/api/new-article/"+key,
		"application/json",
		bytes.NewBuffer(jsonData),
	)
	if err != nil {
		return err
	}

	if resp.StatusCode != http.StatusOK {
		return errors.New("Post to server failed")
	}

	return nil
}
