axios = require('axios');

const ROOT_GITHUB = 'https://api.github.com';

validateUser = (user, pass) => {
 
    return axios.get(ROOT_GITHUB, {
        auth:{
            username: user,
            password: pass,
        }})
        .then(() => true)
        .catch(() => false)
}

fetchInfo = (user, pass, suffix) => {
    
    return axios.get(ROOT_GITHUB + '/users/' + user + suffix, {
        auth: {
            username: user,
            password: pass,
        }})
        .then(response => response.data)
        .catch(err => console.log(err))
}

addInfo = (user, pass, suffix) => {

    return axios.put(ROOT_GITHUB + '/user' + suffix, {}, {
        headers : {
            Accept: 'application / vnd.github.v3 + json',
            'Content-Length' : 0
        },
        auth: {
            username: user,
            password: pass,
        }})
        .then(response => response.status === 204)
        .catch(err => false)
}

deleteInfo = (user, pass, suffix) => {
    
    return axios.delete(ROOT_GITHUB + '/user' + suffix, {
        auth : {
            username: user,
            password: pass,
        }})
        .then(response =>  response.status === 204)
        .catch(err => false)
}

get = (url) => {
    return axios.get(url)
    .then(response => response.data)
    .catch(err => console.log(err))
}

getWithAuth = (suffix, user, pass) => {
    return axios.get(ROOT_GITHUB + suffix, {
        auth : {
            username : user,
            password : pass,
        }})
        .then(response => response.data)
        .catch(err => console.log(err))
}


export default {
    validateUser: validateUser,
    fetchInfo: fetchInfo,
    get: get,
    getWithAuth : getWithAuth,
    addInfo: addInfo,
    deleteInfo: deleteInfo
}