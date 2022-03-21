package org.im97mori.ble.android.peripheral.utils;


import static org.im97mori.ble.constants.CharacteristicUUID.MANUFACTURER_NAME_STRING_CHARACTERISTIC;
import static org.im97mori.ble.constants.DescriptorUUID.CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattDescriptor;
import android.bluetooth.BluetoothGattService;
import android.content.Context;
import android.os.Build;
import android.widget.AutoCompleteTextView;
import android.widget.EditText;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.DescriptorData;
import org.im97mori.ble.MockData;
import org.im97mori.ble.ServiceData;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.LinkedList;

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
public final class UtilsInnerTest {

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
        EditText editText = new EditText(mContext);
        CharSequence original = editText.getText();
        UtilsInner.setTextDistinct(editText, null);
        assertEquals(original.toString(), editText.getText().toString());
    }

    @Test
    public void test_setTextDistinct_00002() {
        EditText editText = new EditText(mContext);
        CharSequence original = editText.getText();
        UtilsInner.setTextDistinct(editText, original);
        assertEquals(original.toString(), editText.getText().toString());
    }

    @Test
    public void test_setTextDistinct_00003() {
        EditText editText = new EditText(mContext);
        CharSequence original = editText.getText();
        CharSequence modified = original + "a";
        UtilsInner.setTextDistinct(editText, modified);
        assertEquals(modified.toString(), editText.getText().toString());
    }

    @Test
    public void test_setTextDistinct_00004() {
        AutoCompleteTextView autoCompleteTextView = new AutoCompleteTextView(mContext);
        CharSequence original = autoCompleteTextView.getText();
        UtilsInner.setTextDistinct(autoCompleteTextView, null);
        assertEquals(original.toString(), autoCompleteTextView.getText().toString());
    }

    @Test
    public void test_setTextDistinct_00005() {
        AutoCompleteTextView autoCompleteTextView = new AutoCompleteTextView(mContext);
        CharSequence original = autoCompleteTextView.getText();
        UtilsInner.setTextDistinct(autoCompleteTextView, original);
        assertEquals(original.toString(), autoCompleteTextView.getText().toString());
    }

    @Test
    public void test_setTextDistinct_00006() {
        AutoCompleteTextView autoCompleteTextView = new AutoCompleteTextView(mContext);
        CharSequence original = autoCompleteTextView.getText();
        CharSequence modified = original + "a";
        UtilsInner.setTextDistinct(autoCompleteTextView, modified);
        assertEquals(modified.toString(), autoCompleteTextView.getText().toString());
    }

    @Test
    public void test_byteToParcelable_00001() {
        MockData mockData = UtilsInner.byteToParcelable(null, MockData.CREATOR);
        assertNull(mockData);
    }

    @Test
    public void test_byteToParcelable_00002() {
        MockData original = new MockData(new LinkedList<>());
        MockData mockData = UtilsInner.byteToParcelable(UtilsInner.parcelableToByteArray(original), MockData.CREATOR);
        assertEquals(original, mockData);
    }

    @Test
    public void test_byteToParcelable_00101() {
        ServiceData serviceData = UtilsInner.byteToParcelable(null, ServiceData.CREATOR);
        assertNull(serviceData);
    }

    @Test
    public void test_byteToParcelable_00102() {
        ServiceData original = new ServiceData(DEVICE_INFORMATION_SERVICE
                , BluetoothGattService.SERVICE_TYPE_PRIMARY
                , new LinkedList<>());
        ServiceData serviceData = UtilsInner.byteToParcelable(UtilsInner.parcelableToByteArray(original), ServiceData.CREATOR);
        assertEquals(original, serviceData);
    }

    @Test
    public void test_byteToParcelable_00201() {
        CharacteristicData characteristicData = UtilsInner.byteToParcelable(null, CharacteristicData.CREATOR);
        assertNull(characteristicData);
    }

    @Test
    public void test_byteToParcelable_00202() {
        CharacteristicData original = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        CharacteristicData characteristicData = UtilsInner.byteToParcelable(UtilsInner.parcelableToByteArray(original), CharacteristicData.CREATOR);
        assertEquals(original, characteristicData);
    }

    @Test
    public void test_byteToParcelable_00301() {
        DescriptorData descriptorData = UtilsInner.byteToParcelable(null, DescriptorData.CREATOR);
        assertNull(descriptorData);
    }

    @Test
    public void test_byteToParcelable_00302() {
        DescriptorData original = new DescriptorData(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR
                , BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , BluetoothGattDescriptor.DISABLE_NOTIFICATION_VALUE);
        DescriptorData descriptorData = UtilsInner.byteToParcelable(UtilsInner.parcelableToByteArray(original), DescriptorData.CREATOR);
        assertEquals(original, descriptorData);
    }

}
