package main

import (
	"flag"
	"html/template"
	"io"
	"os"

	"github.com/labstack/echo/v4"
	"github.com/labstack/echo/v4/middleware"
	"github.com/major-seven/thisinformation/news-web/internal/article"
	"github.com/major-seven/thisinformation/news-web/internal/database"
	"github.com/major-seven/thisinformation/news-web/internal/routes"
	_ "github.com/mattn/go-sqlite3"
)

type Template struct {
	tmpl *template.Template
}

func newTemplate() *Template {
	return &Template{
		tmpl: template.Must(template.New("t").Funcs(template.FuncMap{
			"nth": func(articles []*article.Article, i int) *article.Article {
				if i >= len(articles) {
					return nil
				}
				return articles[i]
			},
		}).ParseGlob("views/*.html")),
	}
}

func (t *Template) Render(w io.Writer, name string, data interface{}, c echo.Context) error {
	return t.tmpl.ExecuteTemplate(w, name, data)
}

type Config struct {
	DbPath  string
	NewsKey string
	Port    string
}

func main() {
	newsKey := os.Getenv("NEWS_KEY")
	var dbPath string
	var port string

	flag.StringVar(&newsKey, "news-key", newsKey, "News API key")
	flag.StringVar(&dbPath, "db-path", "./news.db", "Path to the database file")
	flag.StringVar(&port, "port", "3000", "Port to run the server on")
	flag.Parse()

	if newsKey == "" {
		panic("NEWS_KEY is not set")
	}

	config := Config{
		DbPath:  dbPath,
		NewsKey: newsKey,
	}

	db, err := database.GetDB(config.DbPath)
	defer db.Close()
	if err != nil {
		panic(err)
	}

	data, err := database.GetDataFromDB(db)
	if err != nil {
		panic(err)
	}

	app := echo.New()
	app.Renderer = newTemplate()
	app.Use(middleware.Logger())
	app.Static("/static", "static")
	routes.AddRoutes(app, data, db, config.NewsKey)
	app.Logger.Fatal(app.Start(":" + port))
}
