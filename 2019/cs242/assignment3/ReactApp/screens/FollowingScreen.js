import React from 'react';
import { View, ScrollView, AsyncStorage, RefreshControl, Alert, Text } from 'react-native';
import { Constants } from 'expo';
import { ListItem, Icon } from 'react-native-elements';
import LoadingScreen from './LoadingScreen';
import request from '../utils/Request';
import Swipeout from 'react-native-swipeout';

export default class FollowingScreen extends React.Component {

    constructor() {
        super();
        this.state = {
            following: [],
            loading: true,
            unfollowing: [],
        };
    }
    
    static navigationOptions = ({ navigation }) => {
        const { params = {} } = navigation.state;
        
        return {
            headerTitle: ( <Text style={{color : '#fff'}}> Following </Text>),
            headerRight: ( <Icon name='undo' type='font-awesome' color={'#fff'} disabled={params.undoDisable} onPress={params._undo} />)
        };    
    };
    
    componentDidMount() {
        this._fetchFollowing();
        this.props.navigation.setParams({_undo : this._undo, undoDisable : true})
    }

    _fetchFollowing = () => {
        
        Promise.all([AsyncStorage.getItem('user'), AsyncStorage.getItem('pass')]).then(values => {
           request.fetchInfo(values[0], values[1], '/following').then(response => {

               following = response.map(following => {
                   return {
                       avatar: following.avatar_url,
                       name: following.login,
                       url: following.url
                   };
               })

               if (this.state.unfollowing.length === 0) {
                   this.props.navigation.setParams({ undoDisable: true })
               }
                              
                this.setState({
                    following: following,
                    loading: false
                });

           })
       }).catch(err => console.log(err))
    }
    
    _refresh = () => {
        this.setState({ loading: true });
        this._fetchFollowing();
    }

    _undo = () => {

        this.setState({ loading : true });

        var newUnfollowed = this.state.unfollowing;
        var undoElement = newUnfollowed.pop();
        if (undoElement === undefined) return;

        Promise.all([AsyncStorage.getItem('user'), AsyncStorage.getItem('pass')]).then(values => {
            request.addInfo(values[0], values[1], '/following/' + undoElement).then(() => {
                this.setState({ unfollowing: newUnfollowed })
                this._fetchFollowing();
            })
        }).catch(err => console.log(err))
    }

    _unfollow = (index) => {

        this.setState({ loading : true });

        var newFollowing = this.state.following;
        var newUnfollowed = this.state.unfollowing;
        var removedElement = newFollowing.splice(index, 1)[0];
        newUnfollowed.push(removedElement.name);

        Promise.all([AsyncStorage.getItem('user'), AsyncStorage.getItem('pass')]).then(values => {
            request.deleteInfo(values[0], values[1], '/following/' + removedElement.name).then(() => {

                if (newUnfollowed.length === 1) { this.props.navigation.setParams({ undoDisable: false }) }

                this.setState({ loading : false, following : newFollowing, unfollowing : newUnfollowed });
            })
        }).catch(err => console.log(err))
    }

    _navigate = (url) => {
        this.props.navigation.navigate('UserProfile', {url : url})
    }

    _renderFollowing(following, index) {
        
        var deleteButton = [{
            text: 'unfollow',
            backgroundColor: 'red',
            underlayColor: '#fff',
            onPress: () => this._unfollow(index)
        }]

        return (
            <Swipeout 
                key={index}
                autoClose={true}
                right={ deleteButton } 
                >
                <ListItem
                    title={following.name}
                    leftAvatar={{ source: { uri: following.avatar } }}
                    onPress = {() => this._navigate(following.url)}
                />
            </Swipeout>
        );
    }

    render() {
        if (this.state.loading) { return (<LoadingScreen />); }
        
        return (
            <View style={{ flex: 1, paddingHeight: Constants.statusBarHeight,}}>
                <ScrollView refreshControl = {<RefreshControl refreshing={this.state.loading} onRefresh={this._refresh} />} >
                    { this.state.following.map((following, index) => this._renderFollowing(following, index)) }
                </ScrollView>
            </View>
        );
    }
}