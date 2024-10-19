import React from 'react';
import { View, AsyncStorage, ScrollView, RefreshControl } from 'react-native';
import { ListItem } from 'react-native-elements';
import { Constants } from 'expo';
import request from '../utils/Request';
import LoadingScreen from './LoadingScreen';

const NOTIFICATION_TYPE = {
    'PullRequest' : 'md-git-pull-request',
    'Commit' : 'md-git-commit',
    'Default' : 'md-logo-github',
};

const NOTIFICATION_COLOR = {
    'PullRequest': '#007dff',
    'Commit': '#007dff',
    'Default': '#007dff',
}

export default class NotificationScreen extends React.Component{
    
    constructor() {
        super();
        this.state = {
            loading : true,
            notificationList : [],
            popIndex : -1
        }
    }

    static navigationOptions = {
        title: 'Notification'
    }

    componentDidMount() {
        this._fetchNotifications();
    }

    _refresh = () => {
        this.setState({loading : true});
        this._fetchNotifications();
    }

    _fetchNotifications = () => {
        Promise.all([AsyncStorage.getItem('user'), AsyncStorage.getItem('pass')]).then(values => {
            request.getWithAuth('/notifications', values[0], values[1]).then(values => {
                let notifications = [];
                
                if (values !== []) {
                    notifications = values.map(value => value.subject)
                }

                this.setState( {notificationList : notifications, loading : false } );
            })
        })
    } 

    _renderNotifications = (notification, index) => {

        // Return immediately with invalid input
        if (notification === null) return;

        var iconName = notification.type;

        // Set iconName as Default if lookup fails
        if(NOTIFICATION_TYPE[iconName] === null) {
            iconName = 'Default';
        }

        return (
            <ListItem 
                key={index}
                title={notification.title}
                titleStyle={{fontWeight: 'bold' }}
                leftIcon={{
                    size: 30,
                    name: NOTIFICATION_TYPE[iconName],
                    color: NOTIFICATION_COLOR[iconName],
                    type: 'ionicon'
                }}
                onPress={() => this.setState({popIndex : index})}
            />
        )
    }

    render() {

        if(this.state.loading === true) {
            return (<LoadingScreen />);
        }

        return (
            <View style={{ flex: 1, justifyContent: 'center', paddingHeight: Constants.statusBarHeight, }}>
                <ScrollView
                    refreshControl={<RefreshControl refreshing={this.state.loading} onRefresh={this._refresh} />}>
                    { this.state.notificationList.map((notification, index) => this._renderNotifications(notification, index))}
                </ScrollView>
            </View>
        );
    }

};