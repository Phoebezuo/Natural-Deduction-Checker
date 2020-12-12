exports.readFile = function(e) {
	const file = event.target.files[0];
	const reader = new FileReader();
	return new Promise ((resolve, reject) => {
		reader.onload = event => resolve(event.target.result)
		reader.onerror = error => reject(error)
		reader.readAsText(file)
	})
}
