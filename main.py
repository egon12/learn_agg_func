from dataclasses import dataclass

@dataclass
class P:
    id: int
    name: str
    age: int
    salary: int
    dept: str

data = [
P(1,"Egon",50,1000,"Finance"),
P(2,"Gilbert",50,2000,"Finance"),
P(3,"Nova",37,1500,"HR"),
P(4,"Ryan",78,1800,"HR"),
P(5,"Gone",10,400,"Tech"),
]

