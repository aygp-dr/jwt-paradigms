// Approach with accidental complexity
public List<Customer> getActiveCustomers() {
    Session session = null;
    Transaction tx = null;
    List<Customer> customers = new ArrayList<>();
    
    try {
        session = sessionFactory.openSession();
        tx = session.beginTransaction();
        
        String hql = "FROM Customer c WHERE c.active = :active";
        Query query = session.createQuery(hql);
        query.setParameter("active", true);
        
        customers = query.list();
        tx.commit();
    } catch (Exception e) {
        if (tx != null) tx.rollback();
        throw new RuntimeException("Failed to get active customers", e);
    } finally {
        if (session != null) session.close();
    }
    
    return customers;
}

// Approach with less accidental complexity
public List<Customer> getActiveCustomers() {
    return repository.findByActiveTrue();
}
