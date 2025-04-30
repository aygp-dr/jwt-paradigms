// Java consumer
ConnectionFactory factory = new ConnectionFactory();
factory.setHost("localhost");
Connection connection = factory.newConnection();
Channel channel = connection.createChannel();
channel.queueDeclare("task_queue", true, false, false, null);

DeliverCallback deliverCallback = (consumerTag, delivery) -> {
    String message = new String(delivery.getBody(), "UTF-8");
    JSONObject json = new JSONObject(message);
    processTask(json.getInt("id"), json.getString("action"));
};

channel.basicConsume("task_queue", true, deliverCallback, consumerTag -> {});
