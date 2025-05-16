import { readFileSync } from "node:fs";

const regex = /mul\(\d{1,3},\d{1,3}\)/g;

const sum = arr => arr.length === 0 ? 0 : arr[0] + sum(arr.splice(1));

(() => {
    try {
	console.log(
	    sum(
		readFileSync("input03", { encoding: "utf8" })
		    .match(regex)
		    .map(s => s.match(/\d{1,3}/g).map(d => parseInt(d)))
		    .map(([a, b]) => a * b)
	    )
	);
    } catch (err) {
    }
}) ();
