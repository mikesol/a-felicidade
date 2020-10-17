// micro-delay.js
// https://klank-share.s3.eu-west-1.amazonaws.com/K16029477644118762.js
class MicroDelayProcessor extends AudioWorkletProcessor {
  constructor() {
    super();
    this.buffer = null;
  }
  static get parameterDescriptors() {
    return [
      {
        name: "len",
        defaultValue: 1,
        minValue: 1,
        maxValue: 5000,
        automationRate: "a-rate",
      },
    ];
  }

  process(inputs, outputs, parameters) {
    const input = inputs[0];
    const output = outputs[0];
    if (!input || !input[0] || !output || !output[0]) {
      return true;
    }
    if (this.buffer === null) {
      this.buffer = [];
      for (var j = 0; j < output.length; j++) {
        this.buffer.push([0.0]);
      }
    }
    for (var j = 0; j < output.length; j++) {
      var channel = output[j];
      var ichan = input[j];
      for (var i = 0; i < channel.length; i++) {
        var bufferLength =
          parameters["len"].length > 1
            ? parameters["len"][i]
            : parameters["len"][0];
        channel[i] = this.buffer[j].shift();
        var bl = this.buffer[j].length;
        if (bl + 1 > bufferLength) {
          for (var k = 0; k < bl + 1 - bufferLength; k++) {
            this.buffer[j].shift();
          }
        } else if (bl + 1 < bufferLength) {
          for (var k = 0; k < bufferLength - bl - 1; k++) {
            this.buffer[j].unshift(0.0);
          }
        }
        this.buffer[j].push(ichan[i]);
      }
    }
    return true;
  }
}

registerProcessor("klank-micro-delay", MicroDelayProcessor);
