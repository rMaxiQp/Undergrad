groupBy = (objectArray, property) => {
    return objectArray.reduce(function (acc, obj) {
        var key = obj[property];
        if (!acc[key]) {
            acc[key] = [];
        }
        acc[key].push(obj);
        return acc;
    }, {});
}

count = (objectArray) => {
    return objectArray.reduce(function (acc, obj) {
        if (obj in acc) {
            acc[obj] += 1;
        } else {
            acc[obj] = 1;
        }
        return acc;
    }, {});
}

export default {
    groupBy : groupBy,
    count : count
}