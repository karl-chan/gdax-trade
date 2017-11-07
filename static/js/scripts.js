const formatJSON = (str) => {
    try {
        return JSON.stringify(JSON.parse(str), null, 4);
    } catch (err) {
        return str;
    }
}
