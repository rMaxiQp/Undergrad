import React from 'react';
import { View, AsyncStorage, TextInput, Alert, TouchableOpacity, Text } from 'react-native';
import { Constants, ScreenOrientation } from 'expo';
import request from '../utils/Request';

export default class AuthenticationScreen extends React.Component {

    static navigationOptions = {
        title : 'Welcome',
        headerStyle : { backgroundColor: '#0080FF',},
        headerTintColor: '#FFF',        
    };

    constructor() {
        super();
        
        // Enable Rotation
        ScreenOrientation.allowAsync(ScreenOrientation.Orientation.ALL);

        this.state = {
            user: '',
            pass: '',
        };
    }

    _handleUserSubmit = () => {
        const user = this.state.user;
        const pass = this.state.pass;

        return request.validateUser(user, pass).then(result => {
            if (result === false) {
                Alert.alert('401: Invalid Username/Password');
            } else {
                AsyncStorage.setItem('user', user);
                AsyncStorage.setItem('pass', pass);
                this.props.navigation.navigate('Main');
            }
        })
    }

    render() {
        return (
            < View style = {{
                    paddingHeight: Constants.statusBarHeight,
                    justifyContent: 'center',
                    backgroundColor: '#3399ff',
                    alignItems: 'center',
                    flex: 1,}} >
                <TextInput
                    style={{paddingBottom: '5%', alignSelf: 'center', color: '#fff', fontWeight : 'bold' }}
                    placeholder="Username *"
                    onChangeText={(text) => this.setState({user: text})}
                    />
                <TextInput
                    style={{paddingBottom: '5%', alignSelf: 'center', color: '#fff', fontWeight : 'bold'}}
                    placeholder="Password *"
                    onChangeText={(text) => this.setState({pass: text})}
                    secureTextEntry/>
                <TouchableOpacity onPress = {() => this._handleUserSubmit()}>
                    <Text style={{color: '#ffdb4d', fontWeight : 'bold'}}> Submit </Text>
                </TouchableOpacity>
            </View>
        );
    }
}