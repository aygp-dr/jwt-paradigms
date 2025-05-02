# Python producer
import pika

connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()
channel.queue_declare(queue='task_queue', durable=True)
channel.basic_publish(
    exchange='',
    routing_key='task_queue',
    body='{"id": 123, "action": "process"}',
    properties=pika.BasicProperties(delivery_mode=2)
)
