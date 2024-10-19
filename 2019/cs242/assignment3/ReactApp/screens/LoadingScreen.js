import React from 'react';
import {
  View,
  Image,
} from 'react-native';

export default class LoadingScreen extends React.Component {
  render() {
    return (
        <View>
          <Image
            style={{width:'100%'}}
            source={require('../images/loading.gif')}
          />
        </View>
    );
  }
}