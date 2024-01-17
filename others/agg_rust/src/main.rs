#[allow(dead_code)]
struct P {
    id: u64,
    name: String,
    age: u32,
    salary: i64,
    dept: String,
}

impl P {
    fn new(id: u64, name: &str, age: u32, salary: i64, dept: &str) -> Self {
        Self { id, name: name.to_string(), age, salary, dept: dept.to_string(), }
    }
}

fn main() {
    let data = vec![
        P::new(1, "Egon", 50, 1000, "Finance"),
        P::new(2, "Gilbert", 50, 2000, "Finance"),
        P::new(3, "Nova", 37, 1500, "HR"),
        P::new(4, "Ryan", 78, 1800, "HR"),
        P::new(5, "Gone", 10, 400, "Tech"),
    ];

    let result = data.iter()
        .filter(|p| p.age > 40)
        .map(|p| p.salary)
        .sum::<i64>();

    println!("Total salary: {}", result);
}
