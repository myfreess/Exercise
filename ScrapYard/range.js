const ana = require("./ana.js")

const range = n => ana(
    x => {
	if (x === n) return [];
	return [x, x+1];
    })(0)

module.exports = range;
	
