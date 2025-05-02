// Untyped JavaScript - works in TypeScript
function processData(data) {
    return data.filter(item => item.value > 0)
               .map(item => item.value * 2);
}

// Partially typed - adds some safety
function processData2(data: any[]): number[] {
    return data.filter(item => item.value > 0)
               .map(item => item.value * 2);
}

// Fully typed - maximum safety and documentation
interface DataItem {
    id: string;
    value: number;
    timestamp: Date;
}

function processData3(data: DataItem[]): number[] {
    return data.filter(item => item.value > 0)
               .map(item => item.value * 2);
}

// Test data
const testData = [
    { id: "a1", value: 10, timestamp: new Date() },
    { id: "a2", value: -5, timestamp: new Date() },
    { id: "a3", value: 8, timestamp: new Date() }
];

console.log("Untyped result:", processData(testData));
console.log("Partially typed result:", processData2(testData));
console.log("Fully typed result:", processData3(testData));
