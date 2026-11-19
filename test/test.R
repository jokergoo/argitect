
# strict format/syntax
spec = c(
	"arg",
	"arg!",
	"arg=l",
	"arg=i#",
	"arg=i#{}",
	"arg=i#{2,4}",
	"arg=i@", 
	"arg=i@{}",
	"arg=i@{3}", 
	"arg=i@{3,}", 
	"arg=i@{,3}", 
	"arg=i@{2,3}", 
	"arg=i@{foo, bar}", 
	"arg=i@{foo|bar}",
	"arg=i%", 
	"arg=i%{}",
	"arg=i%{foo, bar}",
	"arg=%{}",
	"arg=%{foo, bar}", 
	"arg=%{foo=i, bar=f}", 
	"arg=%{foo, bar=f}",      # --arg foo=a bar=1.1
	"arg=%{foo=i, bar!}",     # --arg foo=1 bar
	"arg=%{foo=i@, bar=f@}",   # --arg foo=1 bar=1.1
	"arg=%{foo=i@[2], bar=f@[3]}",   # --arg foo=1 bar=1.1

	"arg=%{foo=@{1,2}}",   # --arg foo=1 2 3
	"arg=%{foo=(1,2)}",   # --arg foo=1 2 3
	"arg=%{foo=i#, bar=s#}",  # --arg 1 a
	"arg=%{ foo=i#, bar=s#{} }",  # --arg 1 2 a b
	"arg=%{ foo=i#, bar=s#{} }"  # --arg 1 2 a b
)

# recognize and throw error
spec = c(
	"arg=i{3}",
	"arg=i{3,}",
	"arg=i{,3}",
	"arg=i{2,3}",
	"arg=i{foo, bar}",
	
	"arg={foo, bar}",
	"arg={foo=i, bar=f}",
	"arg={foo, bar=f}",
	"arg={foo=i@, bar=f@}"
)

# recognize and throw error
spec = c(
	"arg=#",
	"arg=@",
	"arg=$",
	"arg=%",
	"arg={}"
)

# override
spec = c(
	"arg=i%{foo=f, bar}",
	"arg=i%{foo=f@, bar}"
)
