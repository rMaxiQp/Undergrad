import { createStackNavigator, createDrawerNavigator } from 'react-navigation';

import DrawerBar from '../components/DrawerBar';

import HomeScreen from '../screens/HomeScreen';
import FollowingScreen from '../screens/FollowingScreen';
import FollowerScreen from '../screens/FollowerScreen';
import RepositoryScreen from '../screens/RepositoryScreen';
import StarScreen from '../screens/StarScreen';
import UserScreen from '../screens/UserScreen';
import NotificationScreen from '../screens/NotificationScreen';
import SearchScreen from '../screens/SearchScreen';
import ProfileScreen from '../screens/ProfileScreen';

const HomeStack = createStackNavigator(
  {
    Home: HomeScreen,
    Follower: FollowerScreen,
    Following: FollowingScreen,
    Repository: RepositoryScreen,
    Star: StarScreen,
    UserProfile: UserScreen
  }, {
    initialRouteName: 'Home',
    defaultNavigationOptions: {
      headerStyle: {
        backgroundColor: '#0080FF',
      },
      headerTintColor: '#FFF',
      headerTitleStyle: {
        fontWeight: 'bold',
      },
    },
  });


const ProfileStack = createStackNavigator( 
  { Profile : ProfileScreen }, 
  { headerMode : 'none' }
  )

const NotificationStack = createStackNavigator(
  {
    Notification : NotificationScreen
  }, {
    defaultNavigationOptions: {
      headerStyle: {
        backgroundColor: '#0080FF',
      },
      headerTintColor: '#FFF',
      headerTitleStyle: {
        fontWeight: 'bold',
      },
    },
  }
)

const SearchStack = createStackNavigator(
  {
    Search : SearchScreen
  }, {
    defaultNavigationOptions: {
      headerStyle: {
        backgroundColor: '#0080FF',
      },
      headerTintColor: '#FFF',
      headerTitleStyle: {
        fontWeight: 'bold',
      },
    },
  })

export default createDrawerNavigator({
  Home : { screen: HomeStack },
  Search: { screen: SearchStack },
  Profile: { screen: ProfileStack },
  Notification: { screen: NotificationStack },
}, {
  contentComponent: DrawerBar,
  drawerType: 'front',
});