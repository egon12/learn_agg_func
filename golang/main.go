package main

import (
	"fmt"
	"github.com/egon12/cols"
)

type P struct {
	ID     int
	Name   string
	Age    int
	Salary int
	Dept   string
}

func main() {
	data := []P{
		{1, "Egon", 50, 1000, "Finance"},
		{2, "Gilbert", 50, 2000, "Finance"},
		{3, "Nova", 37, 1500, "HR"},
		{4, "Ryan", 78, 1800, "HR"},
		{5, "Gone", 10, 400, "Tech"},
	}

	data = FilterAgeAbove40(data)

	fmt.Println("Data: ", MapName(data))
	fmt.Println("Total Salary: ", sumSalaryOldPeople(data))
}

func MapName(data []P) []string {
	var names []string
	for _, v := range data {
		names = append(names, v.Name)
	}
	return names
}

func FilterAgeAbove40(data []P) []P {
	var above40 []P
	for _, v := range data {
		if v.Age > 40 {
			above40 = append(above40, v)
		}
	}
	return above40
}

func sumSalaryOldPeople(data []P) int {
	data = cols.Filter(data, func(v P) bool { return v.Age > 40 })
	salary := cols.Map(data, func(v P) int { return v.Salary })
	return cols.Reduce(salary, func(a, b int) int { return a + b }, 0)
}
