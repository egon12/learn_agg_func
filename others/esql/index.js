const NodeSQLParser = require("node-sql-parser");

const parser = new NodeSQLParser.Parser();

const query = "SELECT id, name, age FROM data WHERE age > 40";
//const query = "SELECT id, name FROM data WHERE id = 1";

const ast = parser.astify(query);

var data = [
    {"id":1,"name":"Egon",   "age":50, "salary":1000, "dept":"Finance"},
    {"id":2,"name":"Gilbert","age":50, "salary":2000, "dept":"Finance"},
    {"id":3,"name":"Nova",   "age":37, "salary":1500, "dept":"HR"},
    {"id":4,"name":"Ryan",   "age":78, "salary":1800, "dept":"HR"},
    {"id":5,"name":"Gone",   "age":10, "salary":400,  "dept":"Tech"},
]

function runQuery(query, data) {
	const ast = parser.astify(query);

	const where = tofilter(ast)
	const select = tomap(ast)
	const agg = toreduce(ast)

	return data
		.filter(where)
		.map(select)
		.reduce(agg, []);
}

function tofilter(ast) {
	if (!ast.where) return r => true

	//console.log(ast.where)
	const { type, operator, left, right } = ast.where

	if (type === "binary_expr") {
		const getter = create_getter(left)
		const value = right.value

		switch(operator) {
			case "=":
				return r => getter(r) === value
			case ">":
				return r => getter(r) > value
			case "<":
				return r => getter(r) < value
			case ">=":
				return r => getter(r) >= value
			case "<=":
				return r => getter(r) <= value
			case "!=":
				return r => getter(r) !== value
		}
	}

	
}

// { expr: { type: 'column_ref', table: null, column: '*' }, as: null }
function tomap(ast) {
	const getter = ast.columns.map( selector =>  {
		const name = selector.expr.column
		const getter = (r) => r[name]
		return {
			name,
			getter
		}
	})

	return function(row) {
		return getter.reduce((r, g) => ({
			...r,
			[g.name]: g.getter(row)
		}), {})
	}
}

function toreduce(ast) {
	return (r, row) => r.concat(row)
}

function create_getter(expr) {
	if (expr.type === "column_ref") {
		return (r) => r[expr.column]
	}
}


console.log(runQuery(query, data))
