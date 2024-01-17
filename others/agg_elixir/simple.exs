defmodule Person do
  defstruct id: 0, name: "", age: 0, salary: 0, dept: ""

  # Constructor function to create a new person
  def new(id, name, age, salary, dept) do
    %Person{id: id, name: name, age: age, salary: salary, dept: dept}
  end
end

data = [
  Person.new(1, "Egon", 50, 1000, "Finance"),
  Person.new(2, "Gilbert", 50, 2000, "Finance"),
  Person.new(3, "Nova", 37, 1500, "HR"),
  Person.new(4, "Ryan", 78, 1800, "HR"),
  Person.new(5, "Gone", 10, 400, "Tech"),
]

IO.puts "Total salary of people over 40 years old:"

data
|> Enum.filter(fn p -> p.age > 40 end)
|> Enum.map(fn p -> p.salary end)
|> Enum.reduce(0, fn s, acc -> s + acc end)
|> IO.puts
