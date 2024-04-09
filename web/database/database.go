package database

import (
	"database/sql"
	"fmt"
	"sort"
	"time"

	"github.com/major-seven/thisinformation/web/article"
	"github.com/major-seven/thisinformation/web/data"
)

const startDate = "2024-04-09"

func GetDB(path string) (*sql.DB, error) {
	db, err := sql.Open("sqlite3", path)
	if err != nil {
		return nil, err
	}

	_, err = db.Exec("CREATE TABLE IF NOT EXISTS articles (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, content TEXT, author TEXT, date TEXT)")
	if err != nil {
		return nil, err
	}

	return db, nil
}

func getNumDays(a, b string) int {
	t1, _ := time.Parse("2006-01-02", a)
	t2, _ := time.Parse("2006-01-02", b)
	return int(t1.Sub(t2).Hours() / 24)
}

func GetDataFromDB(db *sql.DB) (*data.Data, error) {
	d, err := GetTodayData(db)
	if err != nil {
		return nil, err
	}

	dates, err := GetAllDates(db)
	if err != nil {
		return nil, err
	}

	d.Dates = dates
	d.Today = time.Now().Format("2006-01-02")
	d.Edition = getNumDays(d.Today, startDate) + 1

	return d, nil
}

func GetDataByDate(db *sql.DB, date string) (*data.Data, error) {
	d := data.Data{}

	rows, err := db.Query(fmt.Sprintf("SELECT title, content, author, date FROM articles WHERE date = '%s'", date))
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	tmp_articles := []*article.Article{}
	for rows.Next() {
		a := new(article.Article)
		err = rows.Scan(&a.Title, &a.Content, &a.Author, &a.Date)
		if err != nil {
			return nil, err
		}
		tmp_articles = append(tmp_articles, a)
	}

	if len(tmp_articles) > 0 {
		d.CoverStory = tmp_articles[0]
		tmp_articles = tmp_articles[1:]

		for _, a := range tmp_articles {
			if a.Title == "CROSSWORD" {
				d.Crossword = a
				break
			} else {
				d.Articles = append(d.Articles, a)
			}
		}

		sort.Sort(article.SortByContentLength(d.Articles))
	}

	dates, err := GetAllDates(db)
	if err != nil {
		return nil, err
	}

	d.Dates = dates
	d.Today = date
	d.Edition = getNumDays(date, startDate) + 1

	return &d, nil
}

func GetTodayData(db *sql.DB) (*data.Data, error) {
	return GetDataByDate(db, time.Now().Format("2006-01-02"))
}

func GetAllDates(db *sql.DB) ([]string, error) {
	dates := []string{}

	rows, err := db.Query("SELECT DISTINCT date FROM articles ORDER BY date DESC")
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	for rows.Next() {
		var date string
		err = rows.Scan(&date)
		if err != nil {
			return nil, err
		}
		dates = append(dates, date)
	}

	return dates, nil
}
