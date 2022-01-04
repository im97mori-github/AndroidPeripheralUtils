package org.im97mori.ble.android.peripheral.ui.device.setting.u2902;

import static androidx.test.espresso.intent.matcher.IntentMatchers.hasComponent;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasExtra;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_PROPERTIES_TYPE;
import static org.im97mori.ble.constants.DescriptorUUID.CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import android.app.Activity;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.core.util.Pair;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class ClientCharacteristicConfigurationLauncherContractTest {

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    @ApplicationContext
    Context mContext;

    private final ClientCharacteristicConfigurationLauncherContract mClientCharacteristicConfigurationLauncherContract
            = new ClientCharacteristicConfigurationLauncherContract();

    @Before
    public void setUp() {
        mHiltRule.inject();
    }

    @Test
    public void test_createIntent_00001() {
        Pair<String, Integer> pair = Pair.create("a", BluetoothGattCharacteristic.PROPERTY_NOTIFY);
        Intent intent = mClientCharacteristicConfigurationLauncherContract.createIntent(mContext, pair);
        assertTrue(hasComponent(new ComponentName(mContext, ClientCharacteristicConfigurationSettingActivity.class)).matches(intent));
        assertTrue(hasExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), pair.first).matches(intent));
        assertTrue(hasExtra(KEY_PROPERTIES_TYPE, pair.second).matches(intent));
    }

    @Test
    public void test_createIntent_00002() {
        Pair<String, Integer> pair = Pair.create("a", BluetoothGattCharacteristic.PROPERTY_INDICATE);
        Intent intent = mClientCharacteristicConfigurationLauncherContract.createIntent(mContext, pair);
        assertTrue(hasComponent(new ComponentName(mContext, ClientCharacteristicConfigurationSettingActivity.class)).matches(intent));
        assertTrue(hasExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), pair.first).matches(intent));
        assertTrue(hasExtra(KEY_PROPERTIES_TYPE, pair.second).matches(intent));
    }

    @Test
    public void test_parseResult_00001() {
        String original = "a";
        Intent intent = new Intent();
        intent.putExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), original);
        String parsed = mClientCharacteristicConfigurationLauncherContract.parseResult(Activity.RESULT_OK, intent);
        assertNotNull(parsed);
        assertEquals(original, parsed);
    }

    @Test
    public void test_parseResult_00002() {
        assertNull(mClientCharacteristicConfigurationLauncherContract.parseResult(Activity.RESULT_CANCELED, null));
    }

}