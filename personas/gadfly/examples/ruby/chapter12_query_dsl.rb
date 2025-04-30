query = customers.
  where { |c| c.age > 21 && c.status == :active }.
  select(:name, :email).
  order_by(:name).
  limit(10)
