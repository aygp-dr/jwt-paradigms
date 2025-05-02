from customers
where age > 21 and status = 'active'
select name, email
order by name
limit 10
