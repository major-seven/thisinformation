package ollama

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"
)

type prompt struct {
	Model  string `json:"model"`
	Prompt string `json:"prompt"`
	Stream bool   `json:"stream"`
}

type articlePromptResponse struct {
	Model     string `json:"model"`
	CreatedAt string `json:"created_at"`
	Response  string `json:"response"`
	Done      bool   `json:"done"`
}

func (prompt *prompt) sendPrompt() (string, error) {
	jsonData, err := json.Marshal(prompt)
	if err != nil {
		return "", err
	}

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

	if resp.StatusCode != http.StatusOK {
		return "", errors.New("Prompt to LLM failed")
	}

	bodyBytes, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}

	var response articlePromptResponse
	if err := json.Unmarshal(bodyBytes, &response); err != nil {
		return "", err
	}

	return response.Response, nil
}

func SelectNArticlesFromTitles(n int, titles []string) ([]string, error) {
	prompt := prompt{
		Model:  "llama2",
		Prompt: fmt.Sprintf("You are the editor of a large news paper. Select the %d most interesting titles from the following list. Only write the titles, do not add any commentary: %s", n, strings.Join(titles, ", ")),
		Stream: false,
	}

	response, err := prompt.sendPrompt()
	if err != nil {
		return nil, err
	}

	split := strings.Split(response, "\n")
	selected := make([]string, 0)
	for _, line := range split {
		_, title, found := strings.Cut(line, " ")
		if found {
			selected = append(selected, title)
		}
	}

	return selected, nil
}

func CreateArticleFromTitle(title, model *string) (string, error) {
	currentTime := time.Now()
	date := currentTime.Format("2006-01-02")

	prompt := prompt{
		Model:  *model,
		Prompt: fmt.Sprintf("%s: %s", date, *title),
		Stream: false,
	}

	content, err := prompt.sendPrompt()
	if err != nil {
		return "", err
	}

	return content, nil
}

func CreateTitleFromArticle(article *string) (string, error) {
	prompt := prompt{
		Model:  "llama2",
		Prompt: fmt.Sprintf("You are a reporter for a large news paper. Write a title for the following article Only give me one title, do not give me a choice and do not add any commentary: %s", *article),
		Stream: false,
	}

	title, err := prompt.sendPrompt()
	if err != nil {
		return "", err
	}

	return title, nil
}

func CreateAuthorFromArticle(article *string) (string, error) {
	prompt := prompt{
		Model:  "llama2",
		Prompt: fmt.Sprintf("Give me a name for the author of the following article of a reputable news paper. Only reply with the name, do not add anything else.: %s", *article),
		Stream: false,
	}

	author, err := prompt.sendPrompt()
	if err != nil {
		return "", err
	}

	return author, nil
}

func CreateImagePromptFromTitle(title string) (string, error) {
	prompt := prompt{
		Model:  "llama2",
		Prompt: "Create a prompt for an image creation engine to generate a suiting image for the following headline, it should be a grainy black and whity photograph, the prompt should be less than 700 characters: " + title,
		Stream: false,
	}

	imagePrompt, err := prompt.sendPrompt()
	if err != nil {
		return "", err
	}

	return imagePrompt, nil
}

func CreateCrosswordHints() (string, error) {
	prompt := prompt{
		Model:  "llama2",
		Prompt: "Give me a list of 10 crossword hints for words. Do not add any comment, just write the list.",
		Stream: false,
	}

	hints, err := prompt.sendPrompt()
	if err != nil {
		return "", err
	}

	if strings.Contains(hints, "1.") {
		hints = "1." + strings.Split(hints, "1.")[1]
	}

	return hints, nil
}
