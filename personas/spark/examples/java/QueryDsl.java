// Using method chaining in Java
userRepository.where(user -> user.getAge() > 21)
              .and(user -> user.getStatus() == Status.ACTIVE)
              .orderBy("createdAt", Direction.DESC)
              .limit(10);
