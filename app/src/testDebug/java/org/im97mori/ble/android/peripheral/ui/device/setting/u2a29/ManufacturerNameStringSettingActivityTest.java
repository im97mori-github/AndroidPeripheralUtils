package org.im97mori.ble.android.peripheral.ui.device.setting.u2a29;

import android.app.Activity;
import android.app.Instrumentation;
import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.text.TextUtils;
import android.widget.TextView;
import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
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
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.module.ViewModelFactoryFunctionModule;
import org.im97mori.ble.android.peripheral.test.FakeViewModelProviderFactoryFunction;
import org.im97mori.ble.android.peripheral.utils.Utils;
import org.im97mori.ble.characteristic.u2a29.ManufacturerNameString;
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
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.Espresso.pressBack;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.matcher.ViewMatchers.*;
import static org.im97mori.ble.android.peripheral.test.TestUtils.getCurrentMethodName;
import static org.im97mori.ble.constants.CharacteristicUUID.MANUFACTURER_NAME_STRING_CHARACTERISTIC;
import static org.junit.Assert.*;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
@UninstallModules(ViewModelFactoryFunctionModule.class)
public class ManufacturerNameStringSettingActivityTest {

    @Module
    @InstallIn(SingletonComponent.class)
    interface FakeViewModelFactoryFunctionModule {
        @Singleton
        @Provides
        public static Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> bindViewModelProviderFactoryFunction() {
            FakeViewModelProviderFactoryFunction fakeViewModelProviderFactoryFunction = new FakeViewModelProviderFactoryFunction();
            fakeViewModelProviderFactoryFunction.setFakeViewModelClass(ManufacturerNameStringSettingViewModel.class, FakeManufacturerNameStringSettingViewModel.class);
            return fakeViewModelProviderFactoryFunction;
        }
    }

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    private ActivityScenario<ManufacturerNameStringSettingActivity> mScenario;

    private FakeManufacturerNameStringSettingViewModel mViewModel;

    @Inject
    @ApplicationContext
    Context mContext;

    @Before
    public void setUp() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mHiltRule.inject();
    }

    @After
    public void tearDown() {
        mScenario.close();
    }

    @Test
    public void test_title_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));
        onView(withId(R.id.topAppBar)).check(matches(hasDescendant(withText(R.string.manufacturer_name_string))));
    }

    @Test
    public void test_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_isErrorResponse_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        onView(withId(R.id.isErrorResponse)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isErrorResponse_00002() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isErrorResponse)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isErrorResponse_00003() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isErrorResponse)).check(matches(isChecked()));
    }

    @Test
    public void test_updateIsErrorResponse_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsErrorResponseConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isErrorResponse)).perform(click());

        assertTrue(result.get());
    }

    @Test
    public void test_responseCode_visibility_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseCode)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_responseCode_visibility_00002() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseCode)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_responseCode_helper_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.responseCode);
            assertEquals(mContext.getString(R.string.error_response_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_responseCode_prefixText_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.responseCode);
            assertEquals(mContext.getString(R.string.hexadecimal), textInputLayout.getPrefixText());
        });
    }

    @Test
    public void test_responseCode_hint_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);

        onView(withId(R.id.responseCodeEdit)).check(matches(withHint(R.string.response_code)));
    }

    @Test
    public void test_responseCode_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseCodeEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_responseCode_error_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.responseCode)).getError())));
    }

    @Test
    public void test_responseCode_error_00002() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseCode)).getError()).toString()));
    }

    @Test
    public void test_updateResponseCode_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        String original = "1";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateResponseCodeConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.responseCodeEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_responseDelay_helper_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.responseDelay);
            assertEquals(mContext.getString(R.string.response_delay_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_responseDelay_hint_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);

        onView(withId(R.id.responseDelayEdit)).check(matches(withHint(R.string.response_delay)));
    }

    @Test
    public void test_responseDelay_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseDelayEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_responseDelay_error_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.responseDelay)).getError())));
    }

    @Test
    public void test_responseDelay_error_00002() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseDelay)).getError()).toString()));
    }

    @Test
    public void test_updateResponseDelay_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        String original = "1";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateResponseDelayConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.responseDelayEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_menu_save_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isNotEnabled()));
    }

    @Test
    public void test_menu_save_00002() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isEnabled()));
    }

    @Test
    public void test_menu_save_00003() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launchActivityForResult(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).perform(click());

        CharacteristicData characteristicData = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , new ManufacturerNameString("a").getBytes()
                , -1);
        byte[] data = Utils.parcelableToByteArray(characteristicData);
        Intent original = new Intent();
        original.putExtra(MANUFACTURER_NAME_STRING_CHARACTERISTIC.toString(), data);
        mViewModel.mObserveSaveSubject.onNext(original);

        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(Activity.RESULT_OK, activityResult.getResultCode());
        Intent resultData = activityResult.getResultData();
        assertNotNull(resultData);
        assertArrayEquals(data, resultData.getByteArrayExtra(MANUFACTURER_NAME_STRING_CHARACTERISTIC.toString()));
    }

    @Test
    public void test_backPressed_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launchActivityForResult(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        pressBack();
        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        TestCase.assertEquals(Activity.RESULT_CANCELED, activityResult.getResultCode());
    }

    @Test
    public void test_recreate_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_isErrorResponse_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isErrorResponse)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.isErrorResponse)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_responseCode_visibility_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseCode)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseCode)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_responseCode_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseCodeEdit)).check(matches(withText("1")));

        mScenario.recreate();

        onView(withId(R.id.responseCodeEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_recreate_responseCode_error_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseCode)).getError()).toString()));

        mScenario.recreate();

        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseCode)).getError()).toString()));
    }

    @Test
    public void test_recreate_responseDelay_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseDelayEdit)).check(matches(withText("1")));

        mScenario.recreate();

        onView(withId(R.id.responseDelayEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_recreate_responseDelay_error_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseDelay)).getError()).toString()));

        mScenario.recreate();

        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseDelay)).getError()).toString()));
    }

    @Test
    public void test_manufacturerNameString_visibility_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.manufacturerNameString)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_manufacturerNameString_visibility_00002() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.manufacturerNameString)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_manufacturerNameString_helper_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.manufacturerNameString);
            assertEquals(mContext.getString(R.string.utf8s), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_manufacturerNameString_hint_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);

        onView(withId(R.id.manufacturerNameStringEdit)).check(matches(withHint(R.string.manufacturer_name_string)));
    }

    @Test
    public void test_manufacturerNameString_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.manufacturerNameStringEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_manufacturerNameString_error_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.manufacturerNameString)).getError())));
    }

    @Test
    public void test_manufacturerNameString_error_00002() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.manufacturerNameString)).getError()).toString()));
    }

    @Test
    public void test_updateManufacturerNameString_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateManufacturerNameStringConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.manufacturerNameStringEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_recreate_manufacturerNameString_visibility_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.manufacturerNameString)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.manufacturerNameString)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_manufacturerNameString_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.manufacturerNameStringEdit)).check(matches(withText("1")));

        mScenario.recreate();

        onView(withId(R.id.manufacturerNameStringEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_recreate_manufacturerNameString_error_00001() {
        Intent intent = new Intent(mContext, ManufacturerNameStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeManufacturerNameStringSettingViewModel) new ViewModelProvider(activity).get(ManufacturerNameStringSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.manufacturerNameString)).getError()).toString()));

        mScenario.recreate();

        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.manufacturerNameString)).getError()).toString()));

    }

}