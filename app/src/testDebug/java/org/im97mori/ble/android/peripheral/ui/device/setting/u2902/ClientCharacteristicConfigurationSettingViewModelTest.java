package org.im97mori.ble.android.peripheral.ui.device.setting.u2902;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_PROPERTIES_TYPE;
import static org.im97mori.ble.constants.DescriptorUUID.CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattDescriptor;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.SavedStateHandle;

import org.im97mori.ble.DescriptorData;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.test.TestLifeCycleOwner;
import org.im97mori.ble.android.peripheral.utils.Utils;
import org.im97mori.ble.descriptor.u2902.ClientCharacteristicConfiguration;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class ClientCharacteristicConfigurationSettingViewModelTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    @Inject
    DeviceSettingDataSource mDeviceSettingDataSource;

    private FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private ClientCharacteristicConfigurationSettingViewModel mViewModel;

    private SavedStateHandle mSavedStateHandle;

    @Inject
    @ApplicationContext
    Context mContext;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mSavedStateHandle = new SavedStateHandle();
        mFakeDeviceSettingRepository = new FakeDeviceSettingRepository(mDeviceSettingDataSource, mContext);
        mViewModel = new ClientCharacteristicConfigurationSettingViewModel(mSavedStateHandle, mFakeDeviceSettingRepository);
    }

    @After
    public void tearDown() {
        mViewModel.dispose();
        mViewModel = null;
        mFakeDeviceSettingRepository = null;
        mSavedStateHandle = null;
    }

    @Test
    public void test_observeSetup_1_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<Boolean> propertiesReference = new AtomicReference<>();
        AtomicReference<String> propertiesDisabledReference = new AtomicReference<>();
        AtomicReference<String> propertiesEnabledReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeProperties(new TestLifeCycleOwner(), propertiesReference::set);
        mViewModel.observePropertiesDisabled(new TestLifeCycleOwner(), propertiesDisabledReference::set);
        mViewModel.observePropertiesEnabled(new TestLifeCycleOwner(), propertiesEnabledReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        intent.putExtra(KEY_PROPERTIES_TYPE, BluetoothGattCharacteristic.PROPERTY_NOTIFY);
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isErrorResponseReference.get());
        assertFalse(propertiesReference.get());
        assertEquals(mContext.getString(R.string.notification_disabled), propertiesDisabledReference.get());
        assertEquals(mContext.getString(R.string.notification_enabled), propertiesEnabledReference.get());
        assertEquals("0", responseCodeReference.get());
        assertEquals(mContext.getString(R.string.out_of_range), responseCodeErrorStringReference.get());
        assertEquals("0", responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_1_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<Boolean> propertiesReference = new AtomicReference<>();
        AtomicReference<String> propertiesDisabledReference = new AtomicReference<>();
        AtomicReference<String> propertiesEnabledReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeProperties(new TestLifeCycleOwner(), propertiesReference::set);
        mViewModel.observePropertiesDisabled(new TestLifeCycleOwner(), propertiesDisabledReference::set);
        mViewModel.observePropertiesEnabled(new TestLifeCycleOwner(), propertiesEnabledReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        intent.putExtra(KEY_PROPERTIES_TYPE, BluetoothGattCharacteristic.PROPERTY_INDICATE);
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isErrorResponseReference.get());
        assertFalse(propertiesReference.get());
        assertEquals(mContext.getString(R.string.indication_disabled), propertiesDisabledReference.get());
        assertEquals(mContext.getString(R.string.indication_enabled), propertiesEnabledReference.get());
        assertEquals("0", responseCodeReference.get());
        assertEquals(mContext.getString(R.string.out_of_range), responseCodeErrorStringReference.get());
        assertEquals("0", responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_2_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<Boolean> propertiesReference = new AtomicReference<>();
        AtomicReference<String> propertiesDisabledReference = new AtomicReference<>();
        AtomicReference<String> propertiesEnabledReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeProperties(new TestLifeCycleOwner(), propertiesReference::set);
        mViewModel.observePropertiesDisabled(new TestLifeCycleOwner(), propertiesDisabledReference::set);
        mViewModel.observePropertiesEnabled(new TestLifeCycleOwner(), propertiesEnabledReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        DescriptorData descriptorData = new DescriptorData(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR
                , BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE
                , 2
                , 1
                , null);
        intent.putExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), Utils.parcelableToByteArray(descriptorData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertTrue(isErrorResponseReference.get());
        assertFalse(propertiesReference.get());
        assertEquals(mContext.getString(R.string.notification_disabled), propertiesDisabledReference.get());
        assertEquals(mContext.getString(R.string.notification_enabled), propertiesEnabledReference.get());
        assertEquals(String.valueOf(descriptorData.responseCode), responseCodeReference.get());
        assertNull(responseCodeErrorStringReference.get());
        assertEquals(String.valueOf(descriptorData.delay), responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_3_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<Boolean> propertiesReference = new AtomicReference<>();
        AtomicReference<String> propertiesDisabledReference = new AtomicReference<>();
        AtomicReference<String> propertiesEnabledReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeProperties(new TestLifeCycleOwner(), propertiesReference::set);
        mViewModel.observePropertiesDisabled(new TestLifeCycleOwner(), propertiesDisabledReference::set);
        mViewModel.observePropertiesEnabled(new TestLifeCycleOwner(), propertiesEnabledReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        DescriptorData descriptorData = new DescriptorData(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR
                , BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE
                , 2
                , 1
                , new ClientCharacteristicConfiguration(BluetoothGattDescriptor.ENABLE_NOTIFICATION_VALUE).getProperties());
        intent.putExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), Utils.parcelableToByteArray(descriptorData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertTrue(isErrorResponseReference.get());
        assertTrue(propertiesReference.get());
        assertEquals(mContext.getString(R.string.notification_disabled), propertiesDisabledReference.get());
        assertEquals(mContext.getString(R.string.notification_enabled), propertiesEnabledReference.get());
        assertEquals(String.valueOf(descriptorData.responseCode), responseCodeReference.get());
        assertNull(responseCodeErrorStringReference.get());
        assertEquals(String.valueOf(descriptorData.delay), responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_3_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<Boolean> propertiesReference = new AtomicReference<>();
        AtomicReference<String> propertiesDisabledReference = new AtomicReference<>();
        AtomicReference<String> propertiesEnabledReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeProperties(new TestLifeCycleOwner(), propertiesReference::set);
        mViewModel.observePropertiesDisabled(new TestLifeCycleOwner(), propertiesDisabledReference::set);
        mViewModel.observePropertiesEnabled(new TestLifeCycleOwner(), propertiesEnabledReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        DescriptorData descriptorData = new DescriptorData(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR
                , BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE
                , 2
                , 1
                , new ClientCharacteristicConfiguration(BluetoothGattDescriptor.ENABLE_INDICATION_VALUE).getProperties());
        intent.putExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), Utils.parcelableToByteArray(descriptorData));
        intent.putExtra(KEY_PROPERTIES_TYPE, BluetoothGattCharacteristic.PROPERTY_INDICATE);
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertTrue(isErrorResponseReference.get());
        assertTrue(propertiesReference.get());
        assertEquals(mContext.getString(R.string.indication_disabled), propertiesDisabledReference.get());
        assertEquals(mContext.getString(R.string.indication_enabled), propertiesEnabledReference.get());
        assertEquals(String.valueOf(descriptorData.responseCode), responseCodeReference.get());
        assertNull(responseCodeErrorStringReference.get());
        assertEquals(String.valueOf(descriptorData.delay), responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeResponseCode_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> responseCodeReference = new AtomicReference<>();

        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);

        assertNull(responseCodeReference.get());
    }

    @Test
    public void test_observeResponseCode_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicReference<String> responseCodeReference = new AtomicReference<>();

        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.updateResponseCode(original);

        assertEquals(original, responseCodeReference.get());
    }

    @Test
    public void test_observeResponseCode_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicReference<String> responseCodeReference = new AtomicReference<>();

        mViewModel.updateResponseCode(original);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);

        assertEquals(original, responseCodeReference.get());
    }

    @Test
    public void test_observeResponseCode_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> responseCodeReference = new AtomicReference<>();

        mViewModel.observeResponseCode(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            responseCodeReference.set(aBoolean);
        });
        mViewModel.updateResponseCode(original);
        mViewModel.updateResponseCode(original);

        assertEquals(original, responseCodeReference.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeResponseCodeErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetResponseCodeErrorString = "a";
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();

        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);

        assertNull(responseCodeErrorStringReference.get());
    }

    @Test
    public void test_observeResponseCodeErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetResponseCodeErrorString = original;
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();

        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.updateResponseCode("");

        assertEquals(original, responseCodeErrorStringReference.get());
    }

    @Test
    public void test_observeResponseCodeErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetResponseCodeErrorString = original;
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();

        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            responseCodeErrorStringReference.set(s);
        });
        mViewModel.updateResponseCode("");
        mViewModel.updateResponseCode("");

        assertEquals(original, responseCodeErrorStringReference.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeResponseDelay_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> responseDelayReference = new AtomicReference<>();

        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);

        assertNull(responseDelayReference.get());
    }

    @Test
    public void test_observeResponseDelay_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicReference<String> responseDelayReference = new AtomicReference<>();

        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.updateResponseDelay(original);

        assertEquals(original, responseDelayReference.get());
    }

    @Test
    public void test_observeResponseDelay_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicReference<String> responseDelayReference = new AtomicReference<>();

        mViewModel.updateResponseDelay(original);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);

        assertEquals(original, responseDelayReference.get());
    }

    @Test
    public void test_observeResponseDelay_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> responseDelayReference = new AtomicReference<>();

        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            responseDelayReference.set(aBoolean);
        });
        mViewModel.updateResponseDelay(original);
        mViewModel.updateResponseDelay(original);

        assertEquals(original, responseDelayReference.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeResponseDelayErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetResponseCodeErrorString = "a";
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeResponseDelayErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetResponseDelayErrorString = original;
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);
        mViewModel.updateResponseDelay("");

        assertEquals(original, responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeResponseDelayErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetResponseDelayErrorString = original;
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            responseDelayErrorStringReference.set(s);
        });
        mViewModel.updateResponseDelay("");
        mViewModel.updateResponseDelay("");

        assertEquals(original, responseDelayErrorStringReference.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_updateIsErrorResponse_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean after = Boolean.TRUE;

        assertNull(mSavedStateHandle.get("KEY_IS_ERROR_RESPONSE"));
        mViewModel.updateIsErrorResponse(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_ERROR_RESPONSE"));
    }

    @Test
    public void test_updateIsErrorResponse_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean before = Boolean.FALSE;
        Boolean after = Boolean.TRUE;

        mViewModel.updateIsErrorResponse(before);
        assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_IS_ERROR_RESPONSE"));

        mViewModel.updateIsErrorResponse(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_ERROR_RESPONSE"));
    }

    @Test
    public void test_updateResponseCode_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "1";

        assertNull(mSavedStateHandle.get("KEY_RESPONSE_CODE"));
        mViewModel.updateResponseCode(after);

        assertEquals(after, mSavedStateHandle.get("KEY_RESPONSE_CODE"));
    }

    @Test
    public void test_updateResponseCode_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "1";
        String after = "2";

        mViewModel.updateResponseCode(before);
        assertEquals(before, mSavedStateHandle.get("KEY_RESPONSE_CODE"));

        mViewModel.updateResponseCode(after);

        assertEquals(after, mSavedStateHandle.get("KEY_RESPONSE_CODE"));
    }

    @Test
    public void test_updateResponseDelay_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "1";

        assertNull(mSavedStateHandle.get("KEY_RESPONSE_DELAY"));
        mViewModel.updateResponseDelay(after);

        assertEquals(after, mSavedStateHandle.get("KEY_RESPONSE_DELAY"));
    }

    @Test
    public void test_updateResponseDelay_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "1";
        String after = "2";

        mViewModel.updateResponseDelay(before);
        assertEquals(before, mSavedStateHandle.get("KEY_RESPONSE_DELAY"));

        mViewModel.updateResponseDelay(after);

        assertEquals(after, mSavedStateHandle.get("KEY_RESPONSE_DELAY"));
    }

    @Test
    public void test_observeSaveData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Intent> saveDataReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), saveDataReference::set);

        assertNull(saveDataReference.get());
    }

    @Test
    public void test_observeSaveData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent original = new Intent();
        AtomicReference<Intent> saveDataReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), saveDataReference::set);
        mSavedStateHandle.set("KEY_SAVED_DATA", original);

        assertEquals(original, saveDataReference.get());
    }

    @Test
    public void test_save_1_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());
        assertEquals("Already saved", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_1_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay("");

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_1_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateIsErrorResponse(true);
        mViewModel.updateResponseCode("");

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_2_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int responseCode = 1;
        long delay = 2;
        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay(String.valueOf(delay));
        mViewModel.updateIsErrorResponse(true);
        mViewModel.updateResponseCode(String.valueOf(responseCode));

        AtomicReference<DescriptorData> descriptorDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                descriptorDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString()), DescriptorData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        DescriptorData descriptorData = descriptorDataAtomicReference.get();
        assertNotNull(descriptorData);
        assertEquals(delay, descriptorData.delay);
        assertEquals(responseCode, descriptorData.responseCode);
    }

    @Test
    public void test_save_3_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean check = false;
        long delay = 2;
        Intent intent = new Intent();
        intent.putExtra(KEY_PROPERTIES_TYPE, BluetoothGattCharacteristic.PROPERTY_NOTIFY);
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay(String.valueOf(delay));
        mViewModel.updateIsErrorResponse(false);
        mViewModel.updateProperties(check);

        AtomicReference<DescriptorData> descriptorDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                descriptorDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString()), DescriptorData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        DescriptorData descriptorData = descriptorDataAtomicReference.get();
        assertNotNull(descriptorData);
        assertEquals(delay, descriptorData.delay);
        assertEquals(BluetoothGatt.GATT_SUCCESS, descriptorData.responseCode);
        assertEquals(check, new ClientCharacteristicConfiguration(descriptorData.data).isPropertiesNotificationsEnabled());
    }

    @Test
    public void test_save_3_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean check = true;
        long delay = 2;
        Intent intent = new Intent();
        intent.putExtra(KEY_PROPERTIES_TYPE, BluetoothGattCharacteristic.PROPERTY_NOTIFY);
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay(String.valueOf(delay));
        mViewModel.updateIsErrorResponse(false);
        mViewModel.updateProperties(check);

        AtomicReference<DescriptorData> descriptorDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                descriptorDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString()), DescriptorData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        DescriptorData descriptorData = descriptorDataAtomicReference.get();
        assertNotNull(descriptorData);
        assertEquals(delay, descriptorData.delay);
        assertEquals(BluetoothGatt.GATT_SUCCESS, descriptorData.responseCode);
        assertEquals(check, new ClientCharacteristicConfiguration(descriptorData.data).isPropertiesNotificationsEnabled());
    }

    @Test
    public void test_save_3_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean check = false;
        long delay = 2;
        Intent intent = new Intent();
        intent.putExtra(KEY_PROPERTIES_TYPE, BluetoothGattCharacteristic.PROPERTY_INDICATE);
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay(String.valueOf(delay));
        mViewModel.updateIsErrorResponse(false);
        mViewModel.updateProperties(check);

        AtomicReference<DescriptorData> descriptorDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                descriptorDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString()), DescriptorData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        DescriptorData descriptorData = descriptorDataAtomicReference.get();
        assertNotNull(descriptorData);
        assertEquals(delay, descriptorData.delay);
        assertEquals(BluetoothGatt.GATT_SUCCESS, descriptorData.responseCode);
        assertEquals(check, new ClientCharacteristicConfiguration(descriptorData.data).isPropertiesIndicationsEnabled());
    }

    @Test
    public void test_save_3_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean check = true;
        long delay = 2;
        Intent intent = new Intent();
        intent.putExtra(KEY_PROPERTIES_TYPE, BluetoothGattCharacteristic.PROPERTY_INDICATE);
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay(String.valueOf(delay));
        mViewModel.updateIsErrorResponse(false);
        mViewModel.updateProperties(check);

        AtomicReference<DescriptorData> descriptorDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                descriptorDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString()), DescriptorData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        DescriptorData descriptorData = descriptorDataAtomicReference.get();
        assertNotNull(descriptorData);
        assertEquals(delay, descriptorData.delay);
        assertEquals(BluetoothGatt.GATT_SUCCESS, descriptorData.responseCode);
        assertEquals(check, new ClientCharacteristicConfiguration(descriptorData.data).isPropertiesIndicationsEnabled());
    }

    @Test
    public void test_observeProperties_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> propertiesReference = new AtomicReference<>();

        mViewModel.observeProperties(new TestLifeCycleOwner(), propertiesReference::set);

        assertNull(propertiesReference.get());
    }

    @Test
    public void test_observeProperties_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> propertiesReference = new AtomicReference<>();

        mViewModel.observeProperties(new TestLifeCycleOwner(), propertiesReference::set);
        mViewModel.updateProperties(original);

        assertEquals(original, propertiesReference.get().booleanValue());
    }

    @Test
    public void test_observeProperties_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> propertiesReference = new AtomicReference<>();

        mViewModel.updateProperties(original);
        mViewModel.observeProperties(new TestLifeCycleOwner(), propertiesReference::set);

        assertEquals(original, propertiesReference.get().booleanValue());
    }

    @Test
    public void test_observeProperties_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> propertiesReference = new AtomicReference<>();

        mViewModel.observeProperties(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            propertiesReference.set(aBoolean);
        });
        mViewModel.updateProperties(original);
        mViewModel.updateProperties(original);

        assertEquals(original, propertiesReference.get().booleanValue());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observePropertiesDisabled_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> propertiesDisabledReference = new AtomicReference<>();

        mViewModel.observePropertiesDisabled(new TestLifeCycleOwner(), propertiesDisabledReference::set);

        assertNull(propertiesDisabledReference.get());
    }

    @Test
    public void test_observePropertiesDisabled_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> propertiesDisabledReference = new AtomicReference<>();

        mViewModel.observePropertiesDisabled(new TestLifeCycleOwner(), propertiesDisabledReference::set);
        mSavedStateHandle.set("KEY_PROPERTIES_DISABLED", original);

        assertEquals(original, propertiesDisabledReference.get());
    }

    @Test
    public void test_observePropertiesDisabled_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> propertiesDisabledReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_PROPERTIES_DISABLED", original);
        mViewModel.observePropertiesDisabled(new TestLifeCycleOwner(), propertiesDisabledReference::set);

        assertEquals(original, propertiesDisabledReference.get());
    }

    @Test
    public void test_observePropertiesEnabled_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> propertiesEnabledReference = new AtomicReference<>();

        mViewModel.observePropertiesEnabled(new TestLifeCycleOwner(), propertiesEnabledReference::set);

        assertNull(propertiesEnabledReference.get());
    }

    @Test
    public void test_observePropertiesEnabled_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> propertiesEnabledReference = new AtomicReference<>();

        mViewModel.observePropertiesEnabled(new TestLifeCycleOwner(), propertiesEnabledReference::set);
        mSavedStateHandle.set("KEY_PROPERTIES_ENABLED", original);

        assertEquals(original, propertiesEnabledReference.get());
    }

    @Test
    public void test_observePropertiesEnabled_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> propertiesEnabledReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_PROPERTIES_ENABLED", original);
        mViewModel.observePropertiesEnabled(new TestLifeCycleOwner(), propertiesEnabledReference::set);

        assertEquals(original, propertiesEnabledReference.get());
    }

}