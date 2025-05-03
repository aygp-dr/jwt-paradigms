# Using Ruby's block syntax for a query DSL
User.where { age > 21 }.
     and { status == :active }.
     order { created_at.desc }.
     limit(10)
