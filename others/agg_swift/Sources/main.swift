// The Swift Programming Language
// https://docs.swift.org/swift-book


struct P {
    let id: Int
    let name: String
    let age: Int
    let salary: Double
    let dept: String

    init(id: Int, name: String, age: Int, salary: Double, dept: String = "Finance") {
        self.id = id
        self.name = name
        self.age = age
        self.salary = salary
        self.dept = dept
    }
}

var p = [
    P(id: 1, name: "Egon",    age: 50, salary: 1000.0),
    P(id: 2, name: "Gilbert", age: 50, salary: 2000.0),
    P(id: 3, name: "Nova",    age: 37, salary: 1500.0, dept: "HR"),
    P(id: 4, name: "Ryan",    age: 78, salary: 1800.0, dept: "HR"),
    P(id: 5, name: "Gone",    age: 10, salary: 400.0, dept: "Tech"),
]

print("show list")
print(p)

print("show all name")

var strArr = p.map{ $0.name }

print(strArr)

print()
strArr = p.map{ "\($0.name) \($0.salary)" }
print(strArr)

print("show all name and salary")
strArr = p.map{ "\($0.name) \($0.salary)" }

print("sum all salary")
print(p.map{ $0.salary }.reduce(0, +))

print("all names above 40")
print(p.filter{ $0.age > 40 }.map{ $0.name })

print("sum all salary above 40")
print(
    p.filter{ $0.age > 40}
        .map{ $0.salary }
        .reduce(0, +)
)
