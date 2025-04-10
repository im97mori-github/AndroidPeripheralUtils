package org.im97mori.ble.android.peripheral.ui.device.setting.u2a36;

import android.app.Activity;
import android.app.Instrumentation;
import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattDescriptor;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.text.TextUtils;
import android.widget.AutoCompleteTextView;
import android.widget.RadioGroup;
import android.widget.TextView;
import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.core.util.Pair;
import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.core.app.ApplicationProvider;
import androidx.test.espresso.Espresso;
import androidx.test.espresso.intent.Intents;
import androidx.test.espresso.matcher.RootMatchers;
import androidx.test.espresso.matcher.ViewMatchers;
import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.textfield.TextInputLayout;
import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import dagger.hilt.android.testing.UninstallModules;
import dagger.hilt.components.SingletonComponent;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.schedulers.Schedulers;
import junit.framework.TestCase;
import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.DescriptorData;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.module.ViewModelFactoryFunctionModule;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.test.FakeViewModelProviderFactoryFunction;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2902.ClientCharacteristicConfigurationSettingActivity;
import org.im97mori.ble.android.peripheral.utils.Utils;
import org.im97mori.ble.characteristic.core.IEEE_11073_20601_SFLOAT;
import org.im97mori.ble.characteristic.u2a36.IntermediateCuffPressure;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.Espresso.pressBack;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.intent.Intents.intended;
import static androidx.test.espresso.intent.Intents.intending;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasComponent;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasExtra;
import static androidx.test.espresso.matcher.ViewMatchers.*;
import static org.im97mori.ble.android.peripheral.test.TestUtils.getCurrentMethodName;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.*;
import static org.im97mori.ble.constants.CharacteristicUUID.INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
import static org.im97mori.ble.constants.DescriptorUUID.CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
import static org.junit.Assert.*;

@SuppressWarnings("ConstantConditions")
@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
@UninstallModules(ViewModelFactoryFunctionModule.class)
public class IntermediateCuffPressureSettingActivityTest {

    @Module
    @InstallIn(SingletonComponent.class)
    interface FakeViewModelFactoryFunctionModule {
        @Singleton
        @Provides
        public static Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> bindViewModelProviderFactoryFunction() {
            FakeViewModelProviderFactoryFunction fakeViewModelProviderFactoryFunction = new FakeViewModelProviderFactoryFunction();
            fakeViewModelProviderFactoryFunction.setFakeViewModelClass(IntermediateCuffPressureSettingViewModel.class, FakeIntermediateCuffPressureSettingViewModel.class);
            return fakeViewModelProviderFactoryFunction;
        }
    }

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    private ActivityScenario<IntermediateCuffPressureSettingActivity> mScenario;

    private FakeIntermediateCuffPressureSettingViewModel mViewModel;

    @Inject
    @ApplicationContext
    Context mContext;

    @Inject
    FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    @Before
    public void setUp() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mHiltRule.inject();
        Intents.init();
    }

    @After
    public void tearDown() {
        Intents.release();
        mScenario.close();
    }

    @Test
    public void test_title_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));
        onView(withId(R.id.topAppBar)).check(matches(hasDescendant(withText(R.string.intermediate_cuff_pressure))));
    }

    @Test
    public void test_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_menu_save_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isNotEnabled()));
    }

    @Test
    public void test_menu_save_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isEnabled()));
    }

    @Test
    public void test_menu_save_00003() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launchActivityForResult(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).perform(click());

        int intermediateCuffPressureFlags = 0;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        int intermediateCuffPressureMeasurementStatusFlags = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) intermediateCuffPressureMeasurementStatusFlags,
                (byte) (intermediateCuffPressureMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , intermediateCuffPressure.getBytes()
                , -1);
        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR
                , BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , BluetoothGattDescriptor.ENABLE_NOTIFICATION_VALUE);
        intermediateCuffPressureCharacteristicData.descriptorDataList.add(clientCharacteristicConfigurationDescriptorData);

        byte[] data = Utils.parcelableToByteArray(intermediateCuffPressureCharacteristicData);
        Intent original = new Intent();
        original.putExtra(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.toString(), data);
        mViewModel.mObserveSaveSubject.onNext(original);

        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(Activity.RESULT_OK, activityResult.getResultCode());
        Intent resultData = activityResult.getResultData();
        assertNotNull(resultData);
        assertArrayEquals(data, resultData.getByteArrayExtra(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.toString()));
    }

    @Test
    public void test_backPressed_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launchActivityForResult(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        pressBack();
        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        TestCase.assertEquals(Activity.RESULT_CANCELED, activityResult.getResultCode());
    }

    @Test
    public void test_activity_result_00001() {
        Intent resultData = new Intent();
        byte[] after = Utils.parcelableToByteArray(new DescriptorData(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR
                , BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , BluetoothGattDescriptor.DISABLE_NOTIFICATION_VALUE));
        resultData.putExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), after);
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_OK, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ClientCharacteristicConfigurationSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.setClientCharacteristicConfigurationDescriptorData(null);

        mScenario.onActivity(activity -> activity.findViewById(R.id.clientCharacteristicConfigurationSettingButton).performClick());
        Espresso.onIdle();

        assertArrayEquals(after, mViewModel.getClientCharacteristicConfigurationDescriptorData());
    }

    @Test
    public void test_activity_result_00002() {
        Intent resultData = new Intent();
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_CANCELED, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ClientCharacteristicConfigurationSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        byte[] before = Utils.parcelableToByteArray(new DescriptorData(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR
                , BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , BluetoothGattDescriptor.DISABLE_NOTIFICATION_VALUE));
        mViewModel.setClientCharacteristicConfigurationDescriptorData(before);

        mScenario.onActivity(activity -> activity.findViewById(R.id.clientCharacteristicConfigurationSettingButton).performClick());
        Espresso.onIdle();

        assertNull(mViewModel.getClientCharacteristicConfigurationDescriptorData());
    }

    @Test
    public void test_unitRadioGroup_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsMmhgConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            RadioGroup radioGroup = activity.findViewById(R.id.unitRadioGroup);
            radioGroup.check(R.id.mmhgRadioButton);
        });

        assertTrue(result.get());
    }

    @Test
    public void test_recreate_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_unitRadioGroup_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsMmhgConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            RadioGroup radioGroup = activity.findViewById(R.id.unitRadioGroup);
            radioGroup.check(R.id.mmhgRadioButton);
            radioGroup.check(R.id.kpaRadioButton);
        });

        assertFalse(result.get());
    }

    @Test
    public void test_mmhgRadioButton_title_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.mmhgRadioButton)).check(matches(withText(R.string.mmhg)));
    }

    @Test
    public void test_kpaRadioButton_title_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.kpaRadioButton)).check(matches(withText(R.string.kpa)));
    }

    @Test
    public void test_currentCuffPressure_helper_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.currentCuffPressure);
            assertEquals(mContext.getString(R.string.sfloat), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_currentCuffPressure_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.currentCuffPressureEdit)).check(matches(withHint(R.string.current_cuff_pressure)));
    }

    @Test
    public void test_currentCuffPressure_error_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.currentCuffPressure)).getError())));
    }

    @Test
    public void test_currentCuffPressure_error_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.currentCuffPressure)).getError()).toString()));
    }

    @Test
    public void test_updateCurrentCuffPressure_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateCurrentCuffPressureConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.currentCuffPressureEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_isTimeStampSupported_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.isTimeStampSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isTimeStampSupported_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isTimeStampSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isTimeStampSupported_00003() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isTimeStampSupported)).check(matches(isChecked()));
    }

    @Test
    public void test_updateIsTimeStampSupported_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsTimeStampSupportedConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> activity.findViewById(R.id.isTimeStampSupported).performClick());

        assertTrue(result.get());
    }

    @Test
    public void test_timeStampYear_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampYear)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_timeStampYear_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampYear)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_timeStampYear_helper_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.timeStampYear);
            assertEquals(mContext.getString(R.string.time_stamp_year_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_timeStampYear_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampYearEdit)).check(matches(withHint(R.string.year)));
    }

    @Test
    public void test_timeStampYear_error_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.timeStampYear)).getError())));
    }

    @Test
    public void test_timeStampYear_error_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.timeStampYear)).getError()).toString()));
    }

    @Test
    public void test_updateTimeStampYear_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateTimeStampYearConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.timeStampYearEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_timeStampMonth_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampMonth)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_timeStampMonth_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampMonth)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_timeStampMonth_helper_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.timeStampMonth);
            assertEquals(mContext.getString(R.string.time_stamp_month_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_timeStampMonth_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampMonthEdit)).check(matches(withHint(R.string.month)));
    }

    @Test
    public void test_updateTimeStampMonth_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateTimeStampMonthConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.timeStampMonthEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideDateTimeMonthList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_timeStampDay_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampDay)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_timeStampDay_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampDay)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_timeStampDay_helper_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.timeStampDay);
            assertEquals(mContext.getString(R.string.time_stamp_day_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_timeStampDay_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampDayEdit)).check(matches(withHint(R.string.day)));
    }

    @Test
    public void test_updateTimeStampDay_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateTimeStampDayConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.timeStampDayEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideDateTimeDayList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_timeStampHours_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampHours)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_timeStampHours_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampHours)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_timeStampHours_helper_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.timeStampHours);
            assertEquals(mContext.getString(R.string.time_stamp_hours_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_timeStampHours_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampHoursEdit)).check(matches(withHint(R.string.hours)));
    }

    @Test
    public void test_updateTimeStampHours_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateTimeStampHoursConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.timeStampHoursEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideDateTimeHoursList().get(original))).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_timeStampMinutes_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampMinutes)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_timeStampMinutes_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampMinutes)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_timeStampMinutes_helper_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.timeStampMinutes);
            assertEquals(mContext.getString(R.string.time_stamp_minutes_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_timeStampMinutes_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampMinutesEdit)).check(matches(withHint(R.string.minutes)));
    }

    @Test
    public void test_updateTimeStampMinutes_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateTimeStampMinutesConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.timeStampMinutesEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideDateTimeMinutesList().get(original))).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_timeStampSeconds_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampSeconds)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_timeStampSeconds_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampSeconds)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_timeStampSeconds_helper_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.timeStampSeconds);
            assertEquals(mContext.getString(R.string.time_stamp_seconds_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_timeStampSeconds_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampSecondsEdit)).check(matches(withHint(R.string.seconds)));
    }

    @Test
    public void test_updateTimeStampSeconds_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateTimeStampSecondsConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.timeStampSecondsEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideDateTimeMinutesList().get(original))).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_isPulseRateSupported_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsPulseRateSupportedConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> activity.findViewById(R.id.isPulseRateSupported).performClick());

        assertTrue(result.get());
    }

    @Test
    public void test_isPulseRateSupported_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isPulseRateSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isPulseRateSupported_00003() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isPulseRateSupported)).check(matches(isChecked()));
    }

    @Test
    public void test_pulseRate_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.pulseRate)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_pulseRate_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.pulseRate)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_pulseRate_helper_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.pulseRate);
            assertEquals(mContext.getString(R.string.sfloat), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_pulseRate_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.pulseRateEdit)).check(matches(withHint(R.string.pulse_rate)));
    }

    @Test
    public void test_pulseRate_error_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.pulseRate)).getError())));
    }

    @Test
    public void test_pulseRate_error_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.pulseRate)).getError()).toString()));
    }

    @Test
    public void test_updatePulseRate_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdatePulseRateConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.pulseRateEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_isUserIdSupported_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsUserIdSupportedConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> activity.findViewById(R.id.isUserIdSupported).performClick());

        assertTrue(result.get());
    }

    @Test
    public void test_isUserIdSupported_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isUserIdSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isUserIdSupported_00003() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isUserIdSupported)).check(matches(isChecked()));
    }

    @Test
    public void test_userId_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.userId)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_userId_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.userId)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_userId_helper_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.userId);
            assertEquals(mContext.getString(R.string.user_id_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_userId_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.userIdEdit)).check(matches(withHint(R.string.user_id)));
    }

    @Test
    public void test_userId_error_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.userId)).getError())));
    }

    @Test
    public void test_userId_error_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.userId)).getError()).toString()));
    }

    @Test
    public void test_updateUserId_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateUserIdConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.userIdEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_isMeasurementStatusSupported_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsMeasurementStatusSupportedConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> activity.findViewById(R.id.isMeasurementStatusSupported).performClick());

        assertTrue(result.get());
    }

    @Test
    public void test_isMeasurementStatusSupported_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isMeasurementStatusSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isMeasurementStatusSupported_00003() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isMeasurementStatusSupported)).check(matches(isChecked()));
    }

    @Test
    public void test_bodyMovementDetection_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.bodyMovementDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_bodyMovementDetection_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.bodyMovementDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_bodyMovementDetection_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.bodyMovementDetectionEdit)).check(matches(withHint(R.string.body_movement_detection)));
    }

    @Test
    public void test_bodyMovementDetection_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateBodyMovementDetectionConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.bodyMovementDetectionEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideBodyMovementDetectionList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_cuffFitDetection_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.cuffFitDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_cuffFitDetection_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.cuffFitDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_cuffFitDetection_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.cuffFitDetectionEdit)).check(matches(withHint(R.string.cuff_fit_detection)));
    }

    @Test
    public void test_cuffFitDetection_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateCuffFitDetectionConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.cuffFitDetectionEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideCuffFitDetectionList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_irregularPulseDetection_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.irregularPulseDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_irregularPulseDetection_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.irregularPulseDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_irregularPulseDetection_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.irregularPulseDetectionEdit)).check(matches(withHint(R.string.irregular_pulse_detection)));
    }

    @Test
    public void test_irregularPulseDetection_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateIrregularPulseDetectionConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.irregularPulseDetectionEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideIrregularPulseDetectionList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_pulseRateRangeDetection_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.pulseRateRangeDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_pulseRateRangeDetection_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.pulseRateRangeDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_pulseRateRangeDetection_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.pulseRateRangeDetectionEdit)).check(matches(withHint(R.string.pulse_rate_range_detection)));
    }

    @Test
    public void test_pulseRateRangeDetection_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdatePulseRateRangeDetectionConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.pulseRateRangeDetectionEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.providePulseRateRangeDetectionList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_measurementPositionDetection_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.measurementPositionDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_measurementPositionDetection_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.measurementPositionDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_measurementPositionDetection_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.measurementPositionDetectionEdit)).check(matches(withHint(R.string.measurement_position_detection)));
    }

    @Test
    public void test_measurementPositionDetection_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateMeasurementPositionDetectionConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.measurementPositionDetectionEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideMeasurementPositionDetectionList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_clientCharacteristicConfigurationCardView_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.clientCharacteristicConfigurationCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_clientCharacteristicConfigurationCardView_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.clientCharacteristicConfigurationCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_clientCharacteristicConfigurationCardView_title_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.clientCharacteristicConfigurationCardViewTitle)).check(matches(withText(R.string.client_characteristic_configuration)));
    }

    @Test
    public void test_clientCharacteristicConfiguration_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.clientCharacteristicConfiguration)).check(matches(withText("")));
    }

    @Test
    public void test_clientCharacteristicConfiguration_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR
                , BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , BluetoothGattDescriptor.DISABLE_NOTIFICATION_VALUE);
        byte[] originalClientCharacteristicConfigurationDescriptorData = Utils.parcelableToByteArray(clientCharacteristicConfigurationDescriptorData);
        mViewModel.setClientCharacteristicConfigurationDescriptorData(originalClientCharacteristicConfigurationDescriptorData);

        onView(withId(R.id.clientCharacteristicConfiguration))
                .check(matches(withText(mFakeDeviceSettingRepository.getNotificationsDisabledString())));
    }

    @Test
    public void test_clientCharacteristicConfiguration_00003() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR
                , BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , BluetoothGattDescriptor.ENABLE_NOTIFICATION_VALUE);
        byte[] originalClientCharacteristicConfigurationDescriptorData = Utils.parcelableToByteArray(clientCharacteristicConfigurationDescriptorData);
        mViewModel.setClientCharacteristicConfigurationDescriptorData(originalClientCharacteristicConfigurationDescriptorData);

        onView(withId(R.id.clientCharacteristicConfiguration))
                .check(matches(withText(mFakeDeviceSettingRepository.getNotificationsEnabledString())));
    }

    @Test
    public void test_clientCharacteristicConfigurationSettingButton_text_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.clientCharacteristicConfigurationSettingButton)).check(matches(withText(R.string.setting)));
    }

    @Test
    public void test_clientCharacteristicConfigurationSettingButton_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> activity.findViewById(R.id.clientCharacteristicConfigurationSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ClientCharacteristicConfigurationSettingActivity.class)));
        intended(hasExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), (String) null));
    }

    @Test
    public void test_clientCharacteristicConfigurationSettingButton_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> activity.findViewById(R.id.clientCharacteristicConfigurationSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ClientCharacteristicConfigurationSettingActivity.class)));
        intended(hasExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), new byte[]{1}));
    }

    @Test
    public void test_notificationCount_helper_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.notificationCount);
            assertEquals(mContext.getString(R.string.notification_count_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_notificationCount_hint_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.notificationCountEdit)).check(matches(withHint(R.string.notification_count)));
    }

    @Test
    public void test_notificationCount_error_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.notificationCount)).getError())));
    }

    @Test
    public void test_notificationCount_error_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.notificationCount)).getError()).toString()));
    }

    @Test
    public void test_updateNotificationCount_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateNotificationCountConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.notificationCountEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_recreate_unitRadioGroup_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.updateIsMmhg(true);
        mScenario.onActivity(activity -> {
            RadioGroup radioGroup = activity.findViewById(R.id.unitRadioGroup);
            assertEquals(R.id.mmhgRadioButton, radioGroup.getCheckedRadioButtonId());
        });

        mScenario.recreate();

        mScenario.onActivity(activity -> {
            RadioGroup radioGroup = activity.findViewById(R.id.unitRadioGroup);
            assertEquals(R.id.mmhgRadioButton, radioGroup.getCheckedRadioButtonId());
        });
    }

    @Test
    public void test_recreate_unitRadioGroup_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.updateIsMmhg(false);
        mScenario.onActivity(activity -> {
            RadioGroup radioGroup = activity.findViewById(R.id.unitRadioGroup);
            assertEquals(R.id.kpaRadioButton, radioGroup.getCheckedRadioButtonId());
        });

        mScenario.recreate();

        mScenario.onActivity(activity -> {
            RadioGroup radioGroup = activity.findViewById(R.id.unitRadioGroup);
            assertEquals(R.id.kpaRadioButton, radioGroup.getCheckedRadioButtonId());
        });
    }

    @Test
    public void test_recreate_mmhgRadioButton_title_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.mmhgRadioButton)).check(matches(withText(R.string.mmhg)));

        mScenario.recreate();

        onView(withId(R.id.mmhgRadioButton)).check(matches(withText(R.string.mmhg)));
    }

    @Test
    public void test_recreate_kpaRadioButton_title_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.kpaRadioButton)).check(matches(withText(R.string.kpa)));

        mScenario.recreate();

        onView(withId(R.id.kpaRadioButton)).check(matches(withText(R.string.kpa)));
    }

    @Test
    public void test_recreate_currentCuffPressure_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.updateCurrentCuffPressure("1");
        onView(withId(R.id.currentCuffPressureEdit)).check(matches(withText("1")));

        mScenario.recreate();

        onView(withId(R.id.currentCuffPressureEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_recreate_currentCuffPressure_error_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.currentCuffPressure)).getError()).toString()));

        mScenario.recreate();

        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.currentCuffPressure)).getError()).toString()));
    }

    @Test
    public void test_recreate_isTimeStampSupported_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isTimeStampSupported)).check(matches(isNotChecked()));

        mScenario.recreate();

        onView(withId(R.id.isTimeStampSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_recreate_isTimeStampSupported_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isTimeStampSupported)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.isTimeStampSupported)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_timeStampYear_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampYear)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampYear)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.timeStampYear)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.timeStampYear)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_timeStampYear_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampYear)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampYear)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.timeStampYear)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.timeStampYear)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_timeStampYear_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.updateTimeStampYear("5555");
        onView(withId(R.id.timeStampYearEdit)).check(matches(withText("5555")));

        mScenario.recreate();

        onView(withId(R.id.timeStampYearEdit)).check(matches(withText("5555")));
    }

    @Test
    public void test_recreate_timeStampYear_error_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.timeStampYear)).getError()).toString()));

        mScenario.recreate();

        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.timeStampYear)).getError()).toString()));
    }

    @Test
    public void test_recreate_timeStampMonth_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampMonth)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampMonth)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.timeStampMonth)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.timeStampMonth)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_timeStampMonth_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampMonth)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampMonth)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.timeStampMonth)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.timeStampMonth)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_timeStampMonth_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideDateTimeMonthList();
        assertFalse(list.isEmpty());
        int index = list.size() - 1;
        Pair<Integer, String> target = list.get(index);
        mViewModel.updateTimeStampMonth(index);
        onView(withId(R.id.timeStampMonthEdit)).check(matches(withText(target.toString())));

        mScenario.recreate();

        onView(withId(R.id.timeStampMonthEdit)).check(matches(withText(target.toString())));
    }

    @Test
    public void test_recreate_timeStampDay_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampDay)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampDay)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.timeStampDay)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.timeStampDay)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_timeStampDay_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampDay)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampDay)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.timeStampDay)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.timeStampDay)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_timeStampDay_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideDateTimeDayList();
        assertFalse(list.isEmpty());
        int index = list.size() - 1;
        Pair<Integer, String> target = list.get(index);
        mViewModel.updateTimeStampDay(index);
        onView(withId(R.id.timeStampDayEdit)).check(matches(withText(target.toString())));

        mScenario.recreate();

        onView(withId(R.id.timeStampDayEdit)).check(matches(withText(target.toString())));
    }

    @Test
    public void test_recreate_timeStampHours_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampHours)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampHours)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.timeStampHours)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.timeStampHours)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_timeStampHours_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampHours)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampHours)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.timeStampHours)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.timeStampHours)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_timeStampHours_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        List<String> list = mFakeDeviceSettingRepository.provideDateTimeHoursList();
        assertFalse(list.isEmpty());
        int index = list.size() - 1;
        String target = list.get(index);
        mViewModel.updateTimeStampHours(index);
        onView(withId(R.id.timeStampHoursEdit)).check(matches(withText(target)));

        mScenario.recreate();

        onView(withId(R.id.timeStampHoursEdit)).check(matches(withText(target)));
    }

    @Test
    public void test_recreate_timeStampMinutes_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampMinutes)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampMinutes)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.timeStampMinutes)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.timeStampMinutes)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_timeStampMinutes_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampMinutes)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampMinutes)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.timeStampMinutes)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.timeStampMinutes)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_timeStampMinutes_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        List<String> list = mFakeDeviceSettingRepository.provideDateTimeMinutesList();
        assertFalse(list.isEmpty());
        int index = list.size() - 1;
        String target = list.get(index);
        mViewModel.updateTimeStampMinutes(index);
        onView(withId(R.id.timeStampMinutesEdit)).check(matches(withText(target)));

        mScenario.recreate();

        onView(withId(R.id.timeStampMinutesEdit)).check(matches(withText(target)));
    }

    @Test
    public void test_recreate_timeStampSeconds_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampSeconds)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampSeconds)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.timeStampSeconds)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.timeStampSeconds)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_timeStampSeconds_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.timeStampSeconds)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.timeStampSeconds)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.timeStampSeconds)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.timeStampSeconds)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_timeStampSeconds_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        List<String> list = mFakeDeviceSettingRepository.provideDateTimeSecondsList();
        assertFalse(list.isEmpty());
        int index = list.size() - 1;
        String target = list.get(index);
        mViewModel.updateTimeStampSeconds(index);
        onView(withId(R.id.timeStampSecondsEdit)).check(matches(withText(target)));

        mScenario.recreate();

        onView(withId(R.id.timeStampSecondsEdit)).check(matches(withText(target)));
    }

    @Test
    public void test_recreate_isPulseRateSupported_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isPulseRateSupported)).check(matches(isNotChecked()));

        mScenario.recreate();

        onView(withId(R.id.isPulseRateSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_recreate_isPulseRateSupported_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isPulseRateSupported)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.isPulseRateSupported)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_pulseRate_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.pulseRate)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.pulseRate)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.pulseRate)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.pulseRate)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_pulseRate_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.pulseRate)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.pulseRate)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.pulseRate)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.pulseRate)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_pulseRate_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.updatePulseRate("1");
        onView(withId(R.id.pulseRateEdit)).check(matches(withText("1")));

        mScenario.recreate();

        onView(withId(R.id.pulseRateEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_recreate_pulseRate_error_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.pulseRate)).getError()).toString()));

        mScenario.recreate();

        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.pulseRate)).getError()).toString()));
    }

    @Test
    public void test_recreate_isUserIdSupported_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isUserIdSupported)).check(matches(isNotChecked()));

        mScenario.recreate();

        onView(withId(R.id.isUserIdSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_recreate_isUserIdSupported_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isUserIdSupported)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.isUserIdSupported)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_userId_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.userId)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.userId)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.userId)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.userId)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_userId_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.userId)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.userId)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.userId)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.userId)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_userId_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.updateUserId("1");
        onView(withId(R.id.userIdEdit)).check(matches(withText("1")));

        mScenario.recreate();

        onView(withId(R.id.userIdEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_recreate_userId_error_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.userId)).getError()).toString()));

        mScenario.recreate();

        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.userId)).getError()).toString()));
    }

    @Test
    public void test_recreate_isMeasurementStatusSupported_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isMeasurementStatusSupported)).check(matches(isNotChecked()));

        mScenario.recreate();

        onView(withId(R.id.isMeasurementStatusSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_recreate_isMeasurementStatusSupported_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isMeasurementStatusSupported)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.isMeasurementStatusSupported)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_bodyMovementDetection_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.bodyMovementDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.bodyMovementDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.bodyMovementDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.bodyMovementDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_bodyMovementDetection_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.bodyMovementDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.bodyMovementDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.bodyMovementDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.bodyMovementDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_bodyMovementDetection_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideBodyMovementDetectionList();
        assertFalse(list.isEmpty());
        int index = list.size() - 1;
        Pair<Integer, String> target = list.get(index);
        mViewModel.updateBodyMovementDetection(index);
        onView(withId(R.id.bodyMovementDetectionEdit)).check(matches(withText(target.toString())));

        mScenario.recreate();

        onView(withId(R.id.bodyMovementDetectionEdit)).check(matches(withText(target.toString())));
    }

    @Test
    public void test_recreate_cuffFitDetection_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.cuffFitDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.cuffFitDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.cuffFitDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.cuffFitDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_cuffFitDetection_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.cuffFitDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.cuffFitDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.cuffFitDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.cuffFitDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_cuffFitDetection_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideCuffFitDetectionList();
        assertFalse(list.isEmpty());
        int index = list.size() - 1;
        Pair<Integer, String> target = list.get(index);
        mViewModel.updateCuffFitDetection(index);
        onView(withId(R.id.cuffFitDetectionEdit)).check(matches(withText(target.toString())));

        mScenario.recreate();

        onView(withId(R.id.cuffFitDetectionEdit)).check(matches(withText(target.toString())));
    }

    @Test
    public void test_recreate_irregularPulseDetection_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.irregularPulseDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.irregularPulseDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.irregularPulseDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.irregularPulseDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_irregularPulseDetection_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.irregularPulseDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.irregularPulseDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.irregularPulseDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.irregularPulseDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_irregularPulseDetection_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideIrregularPulseDetectionList();
        assertFalse(list.isEmpty());
        int index = list.size() - 1;
        Pair<Integer, String> target = list.get(index);
        mViewModel.updateIrregularPulseDetection(index);
        onView(withId(R.id.irregularPulseDetectionEdit)).check(matches(withText(target.toString())));

        mScenario.recreate();

        onView(withId(R.id.irregularPulseDetectionEdit)).check(matches(withText(target.toString())));
    }

    @Test
    public void test_recreate_pulseRateRangeDetection_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.pulseRateRangeDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.pulseRateRangeDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.pulseRateRangeDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.pulseRateRangeDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_pulseRateRangeDetection_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.pulseRateRangeDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.pulseRateRangeDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.pulseRateRangeDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.pulseRateRangeDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_pulseRateRangeDetection_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.providePulseRateRangeDetectionList();
        assertFalse(list.isEmpty());
        int index = list.size() - 1;
        Pair<Integer, String> target = list.get(index);
        mViewModel.updatePulseRateRangeDetection(index);
        onView(withId(R.id.pulseRateRangeDetectionEdit)).check(matches(withText(target.toString())));

        mScenario.recreate();

        onView(withId(R.id.pulseRateRangeDetectionEdit)).check(matches(withText(target.toString())));
    }

    @Test
    public void test_recreate_measurementPositionDetection_visibility_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.measurementPositionDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.measurementPositionDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.measurementPositionDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.measurementPositionDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_measurementPositionDetection_visibility_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        onView(withId(R.id.measurementPositionDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.measurementPositionDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.measurementPositionDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.measurementPositionDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_measurementPositionDetection_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideMeasurementPositionDetectionList();
        assertFalse(list.isEmpty());
        int index = list.size() - 1;
        Pair<Integer, String> target = list.get(index);
        mViewModel.updateMeasurementPositionDetection(index);
        onView(withId(R.id.measurementPositionDetectionEdit)).check(matches(withText(target.toString())));

        mScenario.recreate();

        onView(withId(R.id.measurementPositionDetectionEdit)).check(matches(withText(target.toString())));
    }

    @Test
    public void test_recreate_clientCharacteristicConfigurationCardView_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.clientCharacteristicConfigurationCardView)).check(matches(isNotChecked()));

        mScenario.recreate();

        onView(withId(R.id.clientCharacteristicConfigurationCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_recreate_clientCharacteristicConfigurationCardView_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.clientCharacteristicConfigurationCardView)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.clientCharacteristicConfigurationCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_clientCharacteristicConfiguration_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR
                , BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , BluetoothGattDescriptor.ENABLE_INDICATION_VALUE);
        byte[] originalClientCharacteristicConfigurationDescriptorData = Utils.parcelableToByteArray(clientCharacteristicConfigurationDescriptorData);
        mViewModel.setClientCharacteristicConfigurationDescriptorData(originalClientCharacteristicConfigurationDescriptorData);

        onView(withId(R.id.clientCharacteristicConfiguration))
                .check(matches(withText(mFakeDeviceSettingRepository.getNotificationsDisabledString())));

        mScenario.recreate();

        onView(withId(R.id.clientCharacteristicConfiguration))
                .check(matches(withText(mFakeDeviceSettingRepository.getNotificationsDisabledString())));
    }

    @Test
    public void test_recreate_clientCharacteristicConfiguration_00002() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR
                , BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , BluetoothGattDescriptor.ENABLE_NOTIFICATION_VALUE);
        byte[] originalClientCharacteristicConfigurationDescriptorData = Utils.parcelableToByteArray(clientCharacteristicConfigurationDescriptorData);
        mViewModel.setClientCharacteristicConfigurationDescriptorData(originalClientCharacteristicConfigurationDescriptorData);

        onView(withId(R.id.clientCharacteristicConfiguration))
                .check(matches(withText(mFakeDeviceSettingRepository.getNotificationsEnabledString())));

        mScenario.recreate();

        onView(withId(R.id.clientCharacteristicConfiguration))
                .check(matches(withText(mFakeDeviceSettingRepository.getNotificationsEnabledString())));
    }

    @Test
    public void test_recreate_notificationCount_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.updateNotificationCount("1");
        onView(withId(R.id.notificationCountEdit)).check(matches(withText("1")));

        mScenario.recreate();

        onView(withId(R.id.notificationCountEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_recreate_notificationCount_error_00001() {
        Intent intent = new Intent(mContext, IntermediateCuffPressureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeIntermediateCuffPressureSettingViewModel) new ViewModelProvider(activity).get(IntermediateCuffPressureSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.notificationCount)).getError()).toString()));

        mScenario.recreate();

        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.notificationCount)).getError()).toString()));
    }

}