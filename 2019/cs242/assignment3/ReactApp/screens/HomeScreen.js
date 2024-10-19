import React from 'react';
import { View, ScrollView, AsyncStorage, Image, TouchableOpacity, RefreshControl } from 'react-native';
import { Constants, ScreenOrientation } from 'expo';
import { ListItem, Icon } from 'react-native-elements';
import LoadingScreen from './LoadingScreen';
import styles from './Screen.style';
import request from '../utils/Request';

const navigationList = [
  {
    name: 'Follower',
    navigation: 'Follower',
    icon: 'android',
    count: 9
  },
  {
    name: 'Following',
    navigation: 'Following',
    icon: 'heart',
    count: 6
  },
  {
    name: 'Repository',
    navigation: 'Repository',
    icon: 'github-square',
    count: 7
  },
  {
    name: 'Star',
    navigation: 'Star',
    icon: 'star',
    count: 8
  }
];

const default_avatar = 'https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png';

export default class HomeScreen extends React.Component {

  constructor() {
    super();
    this.state = {
      navigationList: navigationList,
      loading: true,
    };
  }

  static navigationOptions = ({ navigation }) => {
    const { params = {} } = navigation.state;

    return {
      headerTitle: (<Image source={{ uri: params.avatar ? params.avatar : default_avatar }} style={styles.iconStyle} />),
    };
  };

  componentDidMount() {

    // Enable Rotation
    ScreenOrientation.allowAsync(ScreenOrientation.Orientation.ALL);
        
    this._fetchUserGeneralInfo();
  }

  _mapNavigation = () => {

    return this.state.navigationList.map(
      (navigationItem, index) => 
        <ListItem
          key={index}
          title={navigationItem.name}
          titleStyle={{fontSize: 30,fontWeight: 'bold'}}
          leftIcon={{
            size: 30,
            name: navigationItem.icon,
            type: 'font-awesome'
          }}
          containerStyle={{paddingBottom: '20%'}}
          onPress={() => this.props.navigation.navigate(navigationItem.navigation)}
          badge={{value: navigationItem.count}}
          rightIcon={{
            name: 'angle-right',
            type: 'font-awesome'
        }}/>);
  }

  _profileDeatil = () => {
    this.props.navigation.openDrawer();
  }

  _fetchUserGeneralInfo = () => {

    var navigationList = this.state.navigationList;

    Promise.all([AsyncStorage.getItem('user'), AsyncStorage.getItem('pass')]).then(values => {
      request.fetchInfo(values[0], values[1], '').then(result => {
        request.fetchInfo(values[0], values[1], '/starred').then(stars => {

          navigationList[0].count = result['followers'];
          navigationList[1].count = result['following'];          
          navigationList[2].count = result['public_repos'] + result['owned_private_repos'];
          navigationList[3].count = stars.length;

          // update avatar on header
          this.props.navigation.setParams({ avatar : result['avatar_url']});

          this.setState({navigationList : navigationList, loading : false});
        })
      })
    })
  }

  _refresh = () => {
    this.setState({loading: true});
    this._fetchUserGeneralInfo();
  }

  render() {
    
    if (this.state.loading) { return ( <LoadingScreen />); }
    
    return (
      <View style={{ flex: 1, justifyContent: 'center', paddingHeight: '20px',}}>
        <ScrollView
          refreshControl={ <RefreshControl refreshing={this.state.loading} onRefresh={this._refresh}/>}>
          {this._mapNavigation()}
       </ScrollView>

      </View>
    );
  }
}