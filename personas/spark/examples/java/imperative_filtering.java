List<Person> result = new ArrayList<>();
for (Person p : people) {
    if (p.getAge() > 21) {
        result.add(p);
    }
}
Collections.sort(result, new Comparator<Person>() {
    public int compare(Person p1, Person p2) {
        return p1.getName().compareTo(p2.getName());
    }
});
