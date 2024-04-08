package database

import (
	"database/sql"

	"github.com/major-seven/thisinformation/web/article"
	"github.com/major-seven/thisinformation/web/data"
)

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

func GetDataFromDB(db *sql.DB) (*data.Data, error) {
	d := data.Data{}

	rows, err := db.Query("SELECT title, content, author, date FROM articles")
	if err != nil {
		panic(err)
	}
	defer rows.Close()

	for rows.Next() {
		a := new(article.Article)
		err = rows.Scan(&a.Title, &a.Content, &a.Author, &a.Date)
		if err != nil {
			return nil, err
		}
		d.Articles = append(d.Articles, a)
	}

	return &d, nil
}
