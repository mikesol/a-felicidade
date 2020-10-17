// identity-copy.js
class IdentityCopyProcessor extends AudioWorkletProcessor {
  constructor() {
    super();
  }

  process(inputs, outputs, parameters) {
    const input = inputs[0];
    const output = outputs[0];
    if (!input || !input[0] || !output || !output[0]) {
      return true;
    }
    for (var j = 0; j < output.length; j++) {
      for (var i = 0; i < output[j].length; i++) {
        output[j][i] = input[j][i];
      }
    }
    return true;
  }
}

registerProcessor("klank-identity-copy", IdentityCopyProcessor);
