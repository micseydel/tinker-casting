"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
// Import MQTT library and establish connection parameters
//import * as mqtt from 'mqtt';
const mqtt_1 = __importDefault(require("mqtt"));
const brokerUrl = 'mqtt://localhost:1883'; // Update to your MQTT broker URL
const subscribeTopic = 'my/topic'; // Update to your desired subscription topic
const publishTopic = 'message/length'; // Update to your desired publication topic
// Establish an MQTT client connection
const client = mqtt_1.default.connect(brokerUrl);
// Subscribe to the specified topic and handle incoming messages
client.on('connect', () => {
    console.log(`Connected to ${brokerUrl}`);
    client.subscribe(subscribeTopic, (err) => {
        if (err) {
            console.error(`Error subscribing to ${subscribeTopic}: ${err}`);
        }
        else {
            console.log(`Subscribed to ${subscribeTopic}`);
        }
    });
});
client.on('message', (topic, message) => {
    if (topic === subscribeTopic) {
        const messageLength = message.length;
        client.publish(publishTopic, messageLength.toString(), (err) => {
            if (err) {
                console.error(`Error publishing to ${publishTopic}: ${err}`);
            }
            else {
                console.log(`Published message length (${messageLength}) to ${publishTopic}`);
            }
        });
    }
});
// Handle errors and disconnections
client.on('error', (err) => {
    console.error(`MQTT error: ${err}`);
});
client.on('disconnect', () => {
    console.log('Disconnected from MQTT broker');
});
//# sourceMappingURL=index.js.map