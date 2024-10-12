import React from 'react';
import { View, ScrollView, AsyncStorage } from 'react-native';
import { ListItem } from 'react-native-elements';
import { Constants } from 'expo';
import LoadingScreen from './LoadingScreen';
import request from '../utils/Request';

export default class RepositoryScreen extends React.Component {
    
    static navigationOptions = {
        title: 'Repositories'
    }

    constructor() {
        super();
        this.state = {
            repos: [],
            loading: true,
        };
    }
    
    componentDidMount() {
        this._fetchRepos();
    }

    _fetchRepos = () => {

         Promise.all([AsyncStorage.getItem('user'), AsyncStorage.getItem('pass')]).then(values => {
             request.fetchInfo(values[0], values[1], '/repos').then(response => {

                 repos = response.map(repo => {
                     return {
                         name: repo.name,
                         description: repo.description,
                         url: repo.url,
                         starCount: repo.stargazers_count
                     };
                 })

                 this.setState({
                     repos: repos,
                     loading: false
                 });
             })
         }).catch(err => console.log(err))
    }

    _maprepos = () => {
        return this.state.repos.map( (repo, index) =>
            <ListItem
                key={index}
                title={repo.name}
                subtitle={repo.description}
                badge={{value: repo.starCount, status: 'warning'}} 
                />
            );
    }

    render() {
        if (this.state.loading) {
            return (<LoadingScreen />);
        }

        return (
            <View style={{ flex: 1, paddingHeight: Constants.statusBarHeight,}}>
                <ScrollView>
                    {this._maprepos()}
                </ScrollView>
            </View>
        );
    }
}