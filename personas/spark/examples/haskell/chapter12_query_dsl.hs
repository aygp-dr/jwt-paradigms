query = from customers
        & where_ (\c -> age c > 21 && status c == Active)
        & select [name, email]
        & orderBy name
        & limit 10
