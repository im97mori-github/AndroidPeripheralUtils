package org.im97mori.ble.android.peripheral.utils;

import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.constants.DescriptorUUID.CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
import static org.mockito.Mockito.mockStatic;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattDescriptor;
import android.content.Context;
import android.os.Build;
import android.widget.EditText;

import org.im97mori.ble.DescriptorData;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.concurrent.atomic.AtomicBoolean;

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
public class UtilsTest {

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    @ApplicationContext
    Context mContext;

    @Before
    public void setUp() {
        mHiltRule.inject();
    }

    @Test
    public void test_setTextDistinct_00001() {
        try (MockedStatic<UtilsInner> mockedStatic = mockStatic(UtilsInner.class)) {
            AtomicBoolean result = new AtomicBoolean(false);
            EditText editText = new EditText(mContext);
            mockedStatic.when(() -> UtilsInner.setTextDistinct(editText, null))
                    .thenAnswer(invocation -> {
                        result.set(true);
                        return true;
                    });

            Utils.setTextDistinct(editText, null);
            assertTrue(result.get());
        }
    }

    @Test
    public void test_byteToParcelable_00001() {
        try (MockedStatic<UtilsInner> mockedStatic = mockStatic(UtilsInner.class)) {
            AtomicBoolean result = new AtomicBoolean(false);
            EditText editText = new EditText(mContext);
            mockedStatic.when(() -> UtilsInner.byteToParcelable(null, DescriptorData.CREATOR))
                    .thenAnswer(invocation -> {
                        result.set(true);
                        return true;
                    });

            Utils.byteToParcelable(null, DescriptorData.CREATOR);
            assertTrue(result.get());
        }
    }

    @Test
    public void test_parcelableToByteArray_00001() {
        try (MockedStatic<UtilsInner> mockedStatic = mockStatic(UtilsInner.class)) {
            AtomicBoolean result = new AtomicBoolean(false);
            DescriptorData descriptorData = new DescriptorData(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR
                    , BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE
                    , BluetoothGatt.GATT_SUCCESS
                    , 0
                    , BluetoothGattDescriptor.DISABLE_NOTIFICATION_VALUE);
            mockedStatic.when(() -> UtilsInner.parcelableToByteArray(descriptorData))
                    .thenAnswer(invocation -> {
                        result.set(true);
                        return new byte[0];
                    });

            Utils.parcelableToByteArray(descriptorData);
            assertTrue(result.get());
        }
    }
}