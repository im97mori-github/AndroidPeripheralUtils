package org.im97mori.ble.android.peripheral.ui.device.setting.u2a24;

import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.Espresso.pressBack;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.matcher.ViewMatchers.hasDescendant;
import static androidx.test.espresso.matcher.ViewMatchers.isChecked;
import static androidx.test.espresso.matcher.ViewMatchers.isEnabled;
import static androidx.test.espresso.matcher.ViewMatchers.isNotChecked;
import static androidx.test.espresso.matcher.ViewMatchers.isNotEnabled;
import static androidx.test.espresso.matcher.ViewMatchers.withEffectiveVisibility;
import static androidx.test.espresso.matcher.ViewMatchers.withHint;
import static androidx.test.espresso.matcher.ViewMatchers.withId;
import static androidx.test.espresso.matcher.ViewMatchers.withText;
import static org.im97mori.ble.android.peripheral.test.TestUtils.getCurrentMethodName;
import static org.im97mori.ble.constants.CharacteristicUUID.MODEL_NUMBER_STRING_CHARACTERISTIC;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mockStatic;

import android.app.Activity;
import android.app.Instrumentation;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.text.TextUtils;
import android.widget.TextView;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.espresso.matcher.ViewMatchers;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.textfield.TextInputLayout;
import com.google.gson.Gson;

import junit.framework.TestCase;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;
import org.im97mori.ble.characteristic.u2a24.ModelNumberString;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.Objects;
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
public class ModelNumberStringSettingActivityTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    private ActivityScenario<ModelNumberStringSettingActivity> mScenario;

    private FakeModelNumberStringViewModel mViewModel;

    private static MockedStatic<AutoDisposeViewModelProvider> mockedStatic;

    @Inject
    @ApplicationContext
    Context mContext;

    @Inject
    Gson mGson;

    @BeforeClass
    public static void setUpClass() {
        mockedStatic = mockStatic(AutoDisposeViewModelProvider.class);
        mockedStatic.when(() -> AutoDisposeViewModelProvider.getViewModelClass(ModelNumberStringSettingViewModel.class))
                .thenReturn(FakeModelNumberStringViewModel.class);
    }

    @AfterClass
    public static void tearDownClass() {
        mockedStatic.close();
    }

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
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));
        onView(withId(R.id.topAppBar)).check(matches(hasDescendant(withText(R.string.model_number_string))));
    }

    @Test
    public void test_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_isErrorResponse_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        onView(withId(R.id.isErrorResponse)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isErrorResponse_00002() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isErrorResponse)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isErrorResponse_00003() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isErrorResponse)).check(matches(isChecked()));
    }

    @Test
    public void test_updateIsErrorResponse_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsErrorResponseConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> activity.findViewById(R.id.isErrorResponse).performClick());

        assertTrue(result.get());
    }

    @Test
    public void test_responseCode_visibility_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseCode)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_responseCode_visibility_00002() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseCode)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_responseCode_helper_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.responseCode);
            assertEquals(mContext.getString(R.string.error_response_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_responseCode_prefixText_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.responseCode);
            assertEquals(mContext.getString(R.string.hexadecimal), textInputLayout.getPrefixText());
        });
    }

    @Test
    public void test_responseCode_hint_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);

        onView(withId(R.id.responseCodeEdit)).check(matches(withHint(R.string.response_code)));
    }

    @Test
    public void test_responseCode_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseCodeEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_responseCode_error_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.responseCode)).getError())));
    }

    @Test
    public void test_responseCode_error_00002() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseCode)).getError()).toString()));
    }

    @Test
    public void test_updateResponseCode_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

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
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.responseDelay);
            assertEquals(mContext.getString(R.string.response_delay_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_responseDelay_hint_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);

        onView(withId(R.id.responseDelayEdit)).check(matches(withHint(R.string.response_delay)));
    }

    @Test
    public void test_responseDelay_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseDelayEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_responseDelay_error_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.responseDelay)).getError())));
    }

    @Test
    public void test_responseDelay_error_00002() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseDelay)).getError()).toString()));
    }

    @Test
    public void test_updateResponseDelay_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

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
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isNotEnabled()));
    }

    @Test
    public void test_menu_save_00002() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isEnabled()));
    }

    @Test
    public void test_menu_save_00003() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).perform(click());

        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        characteristicData.data = new ModelNumberString("a").getBytes();
        String json = mGson.toJson(characteristicData);
        Intent original = new Intent();
        original.putExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString(), json);
        mViewModel.mObserveSaveSubject.onNext(original);

        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(Activity.RESULT_OK, activityResult.getResultCode());
        Intent resultData = activityResult.getResultData();
        assertNotNull(resultData);
        assertEquals(json, resultData.getStringExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString()));
    }

    @Test
    public void test_backPressed_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        pressBack();
        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        TestCase.assertEquals(Activity.RESULT_CANCELED, activityResult.getResultCode());
    }

    @Test
    public void test_recreate_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

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
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isErrorResponse)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.isErrorResponse)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_responseCode_visibility_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseCode)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseCode)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_responseCode_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseCodeEdit)).check(matches(withText("1")));

        mScenario.recreate();

        onView(withId(R.id.responseCodeEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_recreate_responseCode_error_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseCode)).getError()).toString()));

        mScenario.recreate();

        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseCode)).getError()).toString()));
    }

    @Test
    public void test_recreate_responseDelay_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.responseDelayEdit)).check(matches(withText("1")));

        mScenario.recreate();

        onView(withId(R.id.responseDelayEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_recreate_responseDelay_error_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseDelay)).getError()).toString()));

        mScenario.recreate();

        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseDelay)).getError()).toString()));
    }

    @Test
    public void test_modelNumberString_visibility_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.modelNumberString)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_modelNumberString_visibility_00002() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.modelNumberString)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_modelNumberString_helper_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.modelNumberString);
            assertEquals(mContext.getString(R.string.utf8s), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_modelNumberString_hint_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);

        onView(withId(R.id.modelNumberStringEdit)).check(matches(withHint(R.string.model_number_string)));
    }

    @Test
    public void test_modelNumberString_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.modelNumberStringEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_modelNumberString_error_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.modelNumberString)).getError())));
    }

    @Test
    public void test_modelNumberString_error_00002() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.modelNumberString)).getError()).toString()));
    }

    @Test
    public void test_updateModelNumberString_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateModelNumberStringConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.modelNumberStringEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_recreate_modelNumberString_visibility_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.modelNumberString)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.modelNumberString)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_modelNumberString_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.modelNumberStringEdit)).check(matches(withText("1")));

        mScenario.recreate();

        onView(withId(R.id.modelNumberStringEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_recreate_modelNumberString_error_00001() {
        Intent intent = new Intent(mContext, ModelNumberStringSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeModelNumberStringViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.modelNumberString)).getError()).toString()));

        mScenario.recreate();

        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.modelNumberString)).getError()).toString()));
    }

}