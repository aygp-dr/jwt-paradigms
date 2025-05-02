List<Person> result = people.stream()
    .filter(p -> p.getAge() > 21)
    .sorted(Comparator.comparing(Person::getName))
    .collect(Collectors.toList());
