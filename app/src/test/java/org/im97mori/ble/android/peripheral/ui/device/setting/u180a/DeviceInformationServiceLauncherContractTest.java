package org.im97mori.ble.android.peripheral.ui.device.setting.u180a;

import static androidx.test.espresso.intent.matcher.IntentMatchers.hasComponent;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasExtra;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

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
public class DeviceInformationServiceLauncherContractTest {

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    @ApplicationContext
    Context mContext;

    private final DeviceInformationServiceLauncherContract mDeviceInformationServiceLauncherContract
            = new DeviceInformationServiceLauncherContract();

    @Before
    public void setUp() {
        mHiltRule.inject();
    }

    @Test
    public void test_createIntent_00001() {
        String original = "a";
        Intent intent = mDeviceInformationServiceLauncherContract.createIntent(mContext, original);
        assertTrue(hasComponent(new ComponentName(mContext, DeviceInformationServiceSettingActivity.class)).matches(intent));
        assertTrue(hasExtra(DEVICE_INFORMATION_SERVICE.toString(), original).matches(intent));
    }

    @Test
    public void test_parseResult_00001() {
        String original = "a";
        Intent intent = new Intent();
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), original);
        String parsed = mDeviceInformationServiceLauncherContract.parseResult(Activity.RESULT_OK, intent);
        assertNotNull(parsed);
        assertEquals(original, parsed);
    }

    @Test
    public void test_parseResult_00002() {
        assertNull(mDeviceInformationServiceLauncherContract.parseResult(Activity.RESULT_CANCELED, null));
    }

}