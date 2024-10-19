import React from 'react';
import { View, ScrollView, AsyncStorage } from 'react-native';
import { ListItem } from 'react-native-elements';
import { Constants } from 'expo';
import LoadingScreen from './LoadingScreen';
import request from '../utils/Request';

export default class FollowerScreen extends React.Component {

    constructor() {
        super();
        this.state = {
            follower: [],
            loading: true,
        };
    }

    static navigationOptions = {
        title: 'Follower'
    }
    
    componentDidMount() {
        this._fetchFollower();
    }

    _fetchFollower = () => {

        Promise.all([AsyncStorage.getItem('user'), AsyncStorage.getItem('pass')]).then(values => {
            request.fetchInfo(values[0], values[1], '/followers').then(response => {
                
                follower = response.map(follower => {
                    return {
                        avatar: follower.avatar_url,
                        name: follower.login,
                        url: follower.url
                    };
                })
                
                AsyncStorage.setItem('follower', follower);

                this.setState({ follower: follower, loading: false });
            })
        }).catch(err => console.log(err))
    }

    _mapFollower = () => {
        return this.state.follower.map( (follower, index) =>
            <ListItem
                key={index}
                title={follower.name}
                leftAvatar={{ source: { uri: follower.avatar } }}
                onPress={() => this.props.navigation.navigate('UserProfile', {url : follower.url})} 
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
                    {this._mapFollower()}
                </ScrollView>
            </View>
        );
    }
}