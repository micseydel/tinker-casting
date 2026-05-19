import mqtt from 'mqtt';

// Configuration
const BROKER_URL = process.env.MQTT_BROKER_URL || 'mqtt://localhost:1883';
const SUBSCRIBE_TOPIC = process.env.SUBSCRIBE_TOPIC || 'test/input';
const PUBLISH_TOPIC = process.env.PUBLISH_TOPIC || 'test/output';
const CLIENT_ID = `mqtt-length-counter-${Math.random().toString(16).slice(2, 8)}`;
const MQTT_USERNAME = 'mosquitto';
const MQTT_PASSWORD = '';

console.log('Starting MQTT Length Counter...');
console.log(`Broker: ${BROKER_URL}`);
console.log(`Subscribe Topic: ${SUBSCRIBE_TOPIC}`);
console.log(`Publish Topic: ${PUBLISH_TOPIC}`);
console.log(`Client ID: ${CLIENT_ID}`);

// Connect to MQTT broker
const client = mqtt.connect(BROKER_URL, {
  clientId: CLIENT_ID,
  clean: true,
  reconnectPeriod: 1000,
  username: MQTT_USERNAME,
  password: MQTT_PASSWORD,
});

// Handle connection
client.on('connect', () => {
  console.log('✓ Connected to MQTT broker');
  
  // Subscribe to input topic
  client.subscribe(SUBSCRIBE_TOPIC, (err) => {
    if (err) {
      console.error('✗ Subscription error:', err);
      process.exit(1);
    }
    console.log(`✓ Subscribed to topic: ${SUBSCRIBE_TOPIC}`);
    console.log('Waiting for messages...\n');
  });
});

// Handle incoming messages
client.on('message', (topic, message) => {
  const messageStr = message.toString();
  const length = messageStr.length;
  
  console.log(`📨 Received message on ${topic}:`);
  console.log(`   Content: "${messageStr}"`);
  console.log(`   Length: ${length} characters`);
  
  // Publish length to output topic
  const lengthMessage = JSON.stringify({
    originalTopic: topic,
    messageLength: length,
    timestamp: new Date().toISOString(),
  });
  
  client.publish(PUBLISH_TOPIC, lengthMessage, (err) => {
    if (err) {
      console.error('✗ Publish error:', err);
    } else {
      console.log(`✓ Published length to ${PUBLISH_TOPIC}\n`);
    }
  });
});

// Handle errors
client.on('error', (err) => {
  console.error('✗ MQTT Error:', err);
});

// Handle disconnection
client.on('close', () => {
  console.log('✗ Disconnected from MQTT broker');
});

// Graceful shutdown
process.on('SIGINT', () => {
  console.log('\nShutting down gracefully...');
  client.end(() => {
    console.log('✓ Disconnected');
    process.exit(0);
  });
});