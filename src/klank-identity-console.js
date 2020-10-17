// identity-console.js
class ConsoleIdentityProcessor extends AudioWorkletProcessor {
  constructor() {
    super();
  }

  process(inputs, outputs, parameters) {
    const input = inputs[0];
    const output = outputs[0];
    console.log("reading");
    if (!input || !input[0] || !output || !output[0]) {
      return true;
    }
    console.log("processing");
    for (var j = 0; j < output.length; j++) {
      output[j] = input[j];
    }
    return true;
  }
}

registerProcessor("klank-identity-console", ConsoleIdentityProcessor);
