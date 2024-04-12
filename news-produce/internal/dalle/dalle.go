package dalle

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"time"
)

type prompt struct {
	Model  string `json:"model"`
	Prompt string `json:"prompt"`
	N      int    `json:"n"`
	Size   string `json:"size"`
}

type imageLink struct {
	Url string `json:"url"`
}

type imageResponse struct {
	Created int         `json:"created"`
	Data    []imageLink `json:"data"`
}

func CreateImageForPrompt(p string) (string, error) {
	key := os.Getenv("OPENAI_KEY")
	if key == "" {
		return "", errors.New("OPENAI_KEY is not set")
	}

	if len(p) > 1000 {
		p = p[:1000]
	}

	prompt := prompt{
		Model:  "dall-e-2",
		Prompt: p,
		N:      1,
		Size:   "1024x1024",
	}

	jsonData, err := json.Marshal(prompt)
	if err != nil {
		return "", err
	}

	req, err := http.NewRequest(
		"POST",
		"https://api.openai.com/v1/images/generations",
		bytes.NewBuffer(jsonData),
	)
	if err != nil {
		return "", err
	}

	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+key)

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	bodyBytes, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}

	if resp.StatusCode != http.StatusOK {
		fmt.Println(resp.StatusCode)
		fmt.Println(string(bodyBytes))
		return "", errors.New("Prompt DALL-E failed")
	}

	var imageResponse imageResponse
	err = json.Unmarshal(bodyBytes, &imageResponse)
	if err != nil {
		return "", err
	}

	return imageResponse.Data[0].Url, nil
}

func DownloadImage(url string, path string) error {
	today := time.Now()

	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	file_name := filepath.Join(path, today.Format("2006-01-02")+".png")
	file, err := os.Create(file_name)
	if err != nil {
		return err
	}
	defer file.Close()

	_, err = io.Copy(file, resp.Body)
	if err != nil {
		return err
	}

	return nil
}

func GenerateDailyPuzzle() (string, error) {
	prompt := "a black and white crossword puzzle top-down flat some letters are already given"
	return CreateImageForPrompt(prompt)
}
