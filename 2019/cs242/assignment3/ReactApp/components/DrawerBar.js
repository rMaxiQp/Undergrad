import React from 'react';
import {
  Text,
  View,
  AsyncStorage,
  ScrollView,
} from 'react-native';
import { DrawerItems } from 'react-navigation';
import LoadingScreen from '../screens/LoadingScreen';
import { ListItem, } from 'react-native-elements';
import request from '../utils/Request';

const default_avatar = 'https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png';

export default class DrawerBar extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      avatar: default_avatar,
      name: '',
      bio: '',
      email: '',
      company: '',
      location: '',
      loading: true,
      props: props,
    }
  }

  componentDidMount() {
    this._fetchUser();
  }
  
  _fetchUser = () => {
    Promise.all([AsyncStorage.getItem('user'), AsyncStorage.getItem('pass')]).then(values => {
      request.fetchInfo(values[0], values[1], '').then(response => {
        this.setState({
          name : response['login'],
          avatar : response['avatar_url'],
          bio : response['bio'],
          email : response['email'],
          company : response['company'],
          location : response['location'],
          loading : false
        })
      })
    }).catch( err => console.log(err))
  }

  _logOut = () => {
    AsyncStorage.setItem('user', '');
    AsyncStorage.setItem('pass', '');
    this.props.navigation.navigate('Auth');
  }

  render () {
    if (this.state.loading) {
      return <LoadingScreen />;
    }

    return (
      <View style={{flex: 1, paddingTop: 10}}>
        <ScrollView>
          <ListItem
            leftAvatar={{ source: { uri: this.state.avatar }, size: 40 }}
            title={this.state.name}
            titleStyle={{ fontSize: 20 }}
            subtitle={this.state.email} />
          <DrawerItems {...this.state.props}/>
        </ScrollView>
        <View>
          <Text style={{ fontSize: 20, backgroundColor: 'red', color: '#FFF'}} onPress={this._logOut}> Log Out </Text>
        </View>
      </View>
    );
  }
}