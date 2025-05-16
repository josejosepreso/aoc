const fs = require('node:fs');

const safe = line => {
    const pairs = line.map((n, idx) => [n, line[idx + 1]] ).filter(a => a[1]);

    const [i, j] = pairs[0][0] > pairs[0][1] ? [0,1] : [1,0];

    for (let k = 0; k < pairs.length; ++k) {
	let pair = pairs[k];
	if (pair[i] - pair[j] > 3
	    || pair[i] === pair[j]
	    || pair[j] > pair[i]
	   )
	    return false;
    }
    
    return true;
}

(() => {
    try {
	console.log(
	    fs.readFileSync("input02", { encoding: "utf8" })
		.split("\n")
		.filter(l => l != "")
		.map(l => l.split(" ").map(e => parseInt(e)))
		.map(safe)
		.filter(b => b)
		.length
	);
    } catch (err) {
    }
})();
