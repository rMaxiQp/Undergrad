import React from 'react';
import { View, AsyncStorage, ScrollView, Text } from 'react-native';
import { Constants } from 'expo';
import request from '../utils/Request';
import LoadingScreen from './LoadingScreen';
import { ListItem } from 'react-native-elements';

export default class ProfileScreen extends React.Component {

    constructor() {
        super();
        this.state = {
            avatar: '',
            bio: '',
            login: '',
            name: '',
            company: '',
            email: '',
            loading: true
        }
    }

    componentDidMount() {
        this._fetchProfile();
    }

    _fetchProfile = () => {
        Promise.all([AsyncStorage.getItem('user'), AsyncStorage.getItem('pass')]).then( values => {
            request.fetchInfo(values[0], values[1], '').then(response => {
                this.setState({
                    avatar: response['avatar_url'],
                    bio: response['bio'],
                    login: response['login'],
                    name: response['name'],
                    company: response['company'],
                    email: response['email'],
                    loading: false
                });
            })
        })
    }

    render() {

        if (this.state.loading === true) {
            return (<LoadingScreen />);
        }

        return (
            <View style={{ flex: 1, justifyContent: 'center', paddingHeight: '20px', }}>
                <ScrollView style={{ flex: 1 }} stickyHeaderIndices={[0]}>
                    <ListItem
                        leftAvatar={{ source: { uri: this.state.avatar }, size: 60 }}
                        title={this.state.login}
                        titleStyle={{ fontSize: 35 }}
                        subtitle={this.state.name} />
                    <Text style={{ alignSelf: 'center' }}>{this.state.bio}</Text>
                    <ListItem
                        leftIcon={{
                            size: 30,
                            name: 'briefcase',
                            type: 'font-awesome'
                        }}
                        title={this.state.company} />
                    <ListItem
                        leftIcon={{
                            size: 30,
                            name: 'map-pin',
                            type: 'font-awesome'
                        }}
                        title={this.state.location} />
                    <ListItem
                        leftIcon={{
                            size: 30,
                            name: 'envelope',
                            type: 'font-awesome'
                        }}
                        title={this.state.email} />
                </ScrollView>
            </View>
        );
    }
};