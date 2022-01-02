package org.im97mori.ble.android.peripheral.ui.device.setting.u2a23;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.constants.CharacteristicUUID.SYSTEM_ID_CHARACTERISTIC;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.test.TestLifeCycleOwner;
import org.im97mori.ble.characteristic.u2a23.SystemId;
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

@SuppressWarnings("ConstantConditions")
@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class SystemIdSettingViewModelTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    @Inject
    DeviceSettingDataSource mDeviceSettingDataSource;

    private FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private SystemIdSettingViewModel mViewModel;

    private SavedStateHandle mSavedStateHandle;

    @Inject
    @ApplicationContext
    Context mContext;

    @Inject
    Gson mGson;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mSavedStateHandle = new SavedStateHandle();
        mFakeDeviceSettingRepository = new FakeDeviceSettingRepository(mDeviceSettingDataSource, mContext);
        mViewModel = new SystemIdSettingViewModel(mSavedStateHandle, mFakeDeviceSettingRepository, mGson);
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
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> manufacturerIdentifierErrorStringReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifier = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeManufacturerIdentifierErrorString(new TestLifeCycleOwner(), manufacturerIdentifierErrorStringReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifier::set);
        mViewModel.observeOrganizationallyUniqueIdentifierErrorString(new TestLifeCycleOwner(), organizationallyUniqueIdentifierErrorStringReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isErrorResponseReference.get());
        assertNull(manufacturerIdentifierReference.get());
        assertEquals(mContext.getString(R.string.no_value), manufacturerIdentifierErrorStringReference.get());
        assertNull(organizationallyUniqueIdentifier.get());
        assertEquals(mContext.getString(R.string.no_value), organizationallyUniqueIdentifierErrorStringReference.get());
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
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> manufacturerIdentifierErrorStringReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifier = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeManufacturerIdentifierErrorString(new TestLifeCycleOwner(), manufacturerIdentifierErrorStringReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifier::set);
        mViewModel.observeOrganizationallyUniqueIdentifierErrorString(new TestLifeCycleOwner(), organizationallyUniqueIdentifierErrorStringReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        characteristicData.responseCode = 2;
        characteristicData.delay = 1;
        intent.putExtra(SYSTEM_ID_CHARACTERISTIC.toString(), mGson.toJson(characteristicData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertTrue(isErrorResponseReference.get());
        assertNull(manufacturerIdentifierReference.get());
        assertEquals(mContext.getString(R.string.no_value), manufacturerIdentifierErrorStringReference.get());
        assertNull(organizationallyUniqueIdentifier.get());
        assertEquals(mContext.getString(R.string.no_value), organizationallyUniqueIdentifierErrorStringReference.get());
        assertEquals(String.valueOf(characteristicData.responseCode), responseCodeReference.get());
        assertNull(responseCodeErrorStringReference.get());
        assertEquals(String.valueOf(characteristicData.delay), responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_3_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> manufacturerIdentifierErrorStringReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifier = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeManufacturerIdentifierErrorString(new TestLifeCycleOwner(), manufacturerIdentifierErrorStringReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifier::set);
        mViewModel.observeOrganizationallyUniqueIdentifierErrorString(new TestLifeCycleOwner(), organizationallyUniqueIdentifierErrorStringReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalManufacturerIdentifier = "1";
        String originalOrganizationallyUniqueIdentifier = "2";
        characteristicData.data = new SystemId(Long.parseLong(originalManufacturerIdentifier), Integer.parseInt(originalOrganizationallyUniqueIdentifier)).getBytes();
        characteristicData.delay = 1;
        intent.putExtra(SYSTEM_ID_CHARACTERISTIC.toString(), mGson.toJson(characteristicData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isErrorResponseReference.get());
        assertEquals(originalManufacturerIdentifier, manufacturerIdentifierReference.get());
        assertNull(manufacturerIdentifierErrorStringReference.get());
        assertEquals(originalOrganizationallyUniqueIdentifier, organizationallyUniqueIdentifier.get());
        assertNull(organizationallyUniqueIdentifierErrorStringReference.get());
        assertEquals(String.valueOf(characteristicData.responseCode), responseCodeReference.get());
        assertEquals(mContext.getString(R.string.out_of_range), responseCodeErrorStringReference.get());
        assertEquals(String.valueOf(characteristicData.delay), responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_4_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Throwable> observeSetupThrowable = new AtomicReference<>();

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        mViewModel.observeSetup(intent
                , () -> {
                }
                , observeSetupThrowable::set);

        assertEquals("Initialized", observeSetupThrowable.get().getMessage());
    }

    @Test
    public void test_observeIsErrorResponse_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);

        assertNull(isErrorResponseReference.get());
    }

    @Test
    public void test_observeIsErrorResponse_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", original);

        assertEquals(original, isErrorResponseReference.get().booleanValue());
    }

    @Test
    public void test_observeIsErrorResponse_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", original);
        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);

        assertEquals(original, isErrorResponseReference.get().booleanValue());
    }

    @Test
    public void test_observeIsErrorResponse_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isErrorResponseReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", original);
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", original);

        assertEquals(original, isErrorResponseReference.get().booleanValue());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeManufacturerIdentifier_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);

        assertNull(manufacturerIdentifierReference.get());
    }

    @Test
    public void test_observeManufacturerIdentifier_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.updateManufacturerIdentifier(original);

        assertEquals(original, manufacturerIdentifierReference.get());
    }

    @Test
    public void test_observeManufacturerIdentifier_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();

        mViewModel.updateManufacturerIdentifier(original);
        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);

        assertEquals(original, manufacturerIdentifierReference.get());
    }

    @Test
    public void test_observeManufacturerIdentifier_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            manufacturerIdentifierReference.set(aBoolean);
        });
        mViewModel.updateManufacturerIdentifier(original);
        mViewModel.updateManufacturerIdentifier(original);

        assertEquals(original, manufacturerIdentifierReference.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeManufacturerIdentifierErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetManufacturerIdentifierErrorString = "a";
        AtomicReference<String> manufacturerIdentifierErrorStringReference = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifierErrorString(new TestLifeCycleOwner(), manufacturerIdentifierErrorStringReference::set);

        assertNull(manufacturerIdentifierErrorStringReference.get());
    }

    @Test
    public void test_observeManufacturerIdentifierErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetManufacturerIdentifierErrorString = original;
        AtomicReference<String> manufacturerIdentifierErrorStringReference = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifierErrorString(new TestLifeCycleOwner(), manufacturerIdentifierErrorStringReference::set);
        mViewModel.updateManufacturerIdentifier(null);

        assertEquals(original, manufacturerIdentifierErrorStringReference.get());
    }

    @Test
    public void test_observeManufacturerIdentifierErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetManufacturerIdentifierErrorString = original;
        AtomicReference<String> manufacturerIdentifierErrorStringReference = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifierErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            manufacturerIdentifierErrorStringReference.set(s);
        });
        mViewModel.updateManufacturerIdentifier(null);
        mViewModel.updateManufacturerIdentifier(null);

        assertEquals(original, manufacturerIdentifierErrorStringReference.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeOrganizationallyUniqueIdentifier_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> organizationallyUniqueIdentifier = new AtomicReference<>();

        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifier::set);

        assertNull(organizationallyUniqueIdentifier.get());
    }

    @Test
    public void test_observeOrganizationallyUniqueIdentifier_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicReference<String> organizationallyUniqueIdentifier = new AtomicReference<>();

        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifier::set);
        mViewModel.updateOrganizationallyUniqueIdentifier(original);

        assertEquals(original, organizationallyUniqueIdentifier.get());
    }

    @Test
    public void test_observeOrganizationallyUniqueIdentifier_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicReference<String> organizationallyUniqueIdentifier = new AtomicReference<>();

        mViewModel.updateOrganizationallyUniqueIdentifier(original);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifier::set);

        assertEquals(original, organizationallyUniqueIdentifier.get());
    }

    @Test
    public void test_observeOrganizationallyUniqueIdentifier_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> organizationallyUniqueIdentifier = new AtomicReference<>();

        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            organizationallyUniqueIdentifier.set(aBoolean);
        });
        mViewModel.updateOrganizationallyUniqueIdentifier(original);
        mViewModel.updateOrganizationallyUniqueIdentifier(original);

        assertEquals(original, organizationallyUniqueIdentifier.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeOrganizationallyUniqueIdentifierErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetOrganizationallyUniqueIdentifierErrorString = "a";
        AtomicReference<String> organizationallyUniqueIdentifierErrorStringReference = new AtomicReference<>();

        mViewModel.observeOrganizationallyUniqueIdentifierErrorString(new TestLifeCycleOwner(), organizationallyUniqueIdentifierErrorStringReference::set);

        assertNull(organizationallyUniqueIdentifierErrorStringReference.get());
    }

    @Test
    public void test_observeOrganizationallyUniqueIdentifierErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetOrganizationallyUniqueIdentifierErrorString = original;
        AtomicReference<String> organizationallyUniqueIdentifierErrorStringReference = new AtomicReference<>();

        mViewModel.observeOrganizationallyUniqueIdentifierErrorString(new TestLifeCycleOwner(), organizationallyUniqueIdentifierErrorStringReference::set);
        mViewModel.updateOrganizationallyUniqueIdentifier(null);

        assertEquals(original, organizationallyUniqueIdentifierErrorStringReference.get());
    }

    @Test
    public void test_observeOrganizationallyUniqueIdentifierErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetOrganizationallyUniqueIdentifierErrorString = original;
        AtomicReference<String> organizationallyUniqueIdentifierErrorStringReference = new AtomicReference<>();

        mViewModel.observeOrganizationallyUniqueIdentifierErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            organizationallyUniqueIdentifierErrorStringReference.set(s);
        });
        mViewModel.updateOrganizationallyUniqueIdentifier(null);
        mViewModel.updateOrganizationallyUniqueIdentifier(null);

        assertEquals(original, organizationallyUniqueIdentifierErrorStringReference.get());
        assertEquals(1, count.get());
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
        mViewModel.updateResponseCode(null);

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
        mViewModel.updateResponseCode(null);
        mViewModel.updateResponseCode(null);

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
        mViewModel.updateResponseDelay(null);

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
        mViewModel.updateResponseDelay(null);
        mViewModel.updateResponseDelay(null);

        assertEquals(original, responseDelayErrorStringReference.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_updateIsErrorResponse_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean after = true;

        assertNull(mSavedStateHandle.get("KEY_IS_ERROR_RESPONSE"));
        mViewModel.updateIsErrorResponse(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_ERROR_RESPONSE").booleanValue());
    }

    @Test
    public void test_updateIsErrorResponse_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean before = false;
        boolean after = true;

        mViewModel.updateIsErrorResponse(before);
        assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_IS_ERROR_RESPONSE").booleanValue());

        mViewModel.updateIsErrorResponse(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_ERROR_RESPONSE").booleanValue());
    }

    @Test
    public void test_updateManufacturerIdentifier_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "1";

        assertNull(mSavedStateHandle.get("KEY_MANUFACTURER_IDENTIFIER"));
        mViewModel.updateManufacturerIdentifier(after);

        assertEquals(after, mSavedStateHandle.get("KEY_MANUFACTURER_IDENTIFIER"));
    }

    @Test
    public void test_updateManufacturerIdentifier_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "1";
        String after = "2";

        mViewModel.updateManufacturerIdentifier(before);
        assertEquals(before, mSavedStateHandle.get("KEY_MANUFACTURER_IDENTIFIER"));

        mViewModel.updateManufacturerIdentifier(after);

        assertEquals(after, mSavedStateHandle.get("KEY_MANUFACTURER_IDENTIFIER"));
    }

    @Test
    public void test_updateOrganizationallyUniqueIdentifier_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "1";

        assertNull(mSavedStateHandle.get("KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER"));
        mViewModel.updateOrganizationallyUniqueIdentifier(after);

        assertEquals(after, mSavedStateHandle.get("KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER"));
    }

    @Test
    public void test_updateOrganizationallyUniqueIdentifier_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "1";
        String after = "2";

        mViewModel.updateOrganizationallyUniqueIdentifier(before);
        assertEquals(before, mSavedStateHandle.get("KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER"));

        mViewModel.updateOrganizationallyUniqueIdentifier(after);

        assertEquals(after, mSavedStateHandle.get("KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER"));
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
    public void test_observeSave_1_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());
        assertEquals("Already saved", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00002() {
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
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00003() {
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
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateIsErrorResponse(false);
        mViewModel.updateManufacturerIdentifier("");
        mViewModel.updateOrganizationallyUniqueIdentifier("1");

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateIsErrorResponse(false);
        mViewModel.updateManufacturerIdentifier("1");
        mViewModel.updateOrganizationallyUniqueIdentifier("");

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_2_00001() {
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

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> characteristicDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(SYSTEM_ID_CHARACTERISTIC.toString()), CharacteristicData.class))
                , throwable -> {
                });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        assertNotNull(characteristicData);
        assertEquals(delay, characteristicData.delay);
        assertEquals(responseCode, characteristicData.responseCode);
    }

    @Test
    public void test_observeSave_3_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String manufacturerIdentifier = "1";
        String organizationallyUniqueIdentifier = "2";
        long delay = 3;
        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay(String.valueOf(delay));
        mViewModel.updateIsErrorResponse(false);
        mViewModel.updateManufacturerIdentifier(manufacturerIdentifier);
        mViewModel.updateOrganizationallyUniqueIdentifier(organizationallyUniqueIdentifier);

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> characteristicDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(SYSTEM_ID_CHARACTERISTIC.toString()), CharacteristicData.class))
                , throwable -> {
                });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        assertNotNull(characteristicData);
        assertEquals(delay, characteristicData.delay);
        assertEquals(BluetoothGatt.GATT_SUCCESS, characteristicData.responseCode);
        SystemId systemId = new SystemId(characteristicData.data);
        assertEquals(manufacturerIdentifier, String.valueOf(systemId.getManufacturerIdentifier()));
        assertEquals(organizationallyUniqueIdentifier, String.valueOf(systemId.getOrganizationallyUniqueIdentifier()));
    }

}