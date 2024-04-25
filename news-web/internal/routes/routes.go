package routes

import (
	"database/sql"
	"net/http"
	"time"

	"github.com/labstack/echo/v4"
	"github.com/major-seven/thisinformation/news-web/internal/article"
	"github.com/major-seven/thisinformation/news-web/internal/data"
	"github.com/major-seven/thisinformation/news-web/internal/database"
)

func AddRoutes(app *echo.Echo, data *data.Data, db *sql.DB, newsKey string) {
	app.GET("/", func(c echo.Context) error {
		data.Views += 1
		database.SaveViews(db, data.Today, data.Views)
		return c.Render(http.StatusOK, "index.html", data)
	})

	app.GET("/archive", func(c echo.Context) error {
		return c.Render(http.StatusOK, "archive.html", data)
	})

	app.GET("/archive/:date", func(c echo.Context) error {
		date := c.Param("date")
		d, err := database.GetDataByDate(db, date)
		if err != nil || len(d.Articles) == 0 {
			return c.Render(http.StatusOK, "invalid-date.html", nil)
		}

		return c.Render(http.StatusOK, "index.html", d)
	})

	app.POST("/api/new-article", func(c echo.Context) error {
		key := c.Request().Header.Get("Authorization")

		if key == newsKey {
			a := new(article.Article)
			if err := c.Bind(a); err != nil {
				return c.String(http.StatusBadRequest, "bad request")
			}

			if a.Title == "" || a.Content == "" || a.Author == "" {
				return c.String(http.StatusBadRequest, "bad request")
			}

			time := time.Now()
			a.Date = time.Format("2006-01-02")

			_, err := db.Exec("INSERT INTO articles (title, content, author, date) VALUES (?, ?, ?, ?)", a.Title, a.Content, a.Author, a.Date)
			if err != nil {
				return c.String(http.StatusInternalServerError, "internal server error")
			}

			newData, err := database.GetTodayData(db)
			if err != nil {
				return c.String(http.StatusInternalServerError, "internal server error")
			}

			data = newData
			return c.String(http.StatusOK, "ok")

		} else {
			return c.String(http.StatusUnauthorized, "unauthorized")
		}
	})

	app.POST("/api/new-articles/:key", func(c echo.Context) error {
		if c.Param("key") == newsKey {
			articles := new(article.ArticleList)
			if err := c.Bind(&articles); err != nil {
				return c.String(http.StatusBadRequest, "bad request")
			}

			if len(articles.Articles) == 0 {
				return c.String(http.StatusBadRequest, "bad request")
			}

			for _, a := range articles.Articles {
				if a.Title == "" || a.Content == "" {
					return c.String(http.StatusBadRequest, "bad request")
				}
			}

			for _, a := range articles.Articles {
				_, err := db.Exec("INSERT INTO articles (title, content, author) VALUES (?, ?, ?)", a.Title, a.Content, a.Author)
				if err != nil {
					return c.String(http.StatusInternalServerError, "internal server error")
				}
			}

			newData, err := database.GetTodayData(db)
			if err != nil {
				return c.String(http.StatusInternalServerError, "internal server error")
			}
			data = newData

			return c.String(http.StatusOK, "ok")

		} else {
			return c.String(http.StatusUnauthorized, "unauthorized")
		}
	})

	app.DELETE("/api/delete-articles/:key", func(c echo.Context) error {
		if c.Param("key") == newsKey {
			data.Articles = nil
			_, err := db.Exec("DELETE FROM articles")
			if err != nil {
				return c.String(http.StatusInternalServerError, "internal server error")
			}

			return c.String(http.StatusOK, "ok")
		} else {
			return c.String(http.StatusUnauthorized, "unauthorized")
		}
	})
}
